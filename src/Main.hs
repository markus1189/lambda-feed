{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Exception (bracket, SomeException, try)
import           Control.Lens (from, view, non, (%~), (&), to)
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Advanced (update', query')
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Text.Lens (_Text)
import           Data.Time (formatTime, defaultTimeLocale, rfc822DateFormat)
import           Data.Time (getCurrentTime, UTCTime)
import           Formatting (sformat, left, (%), (%.), stext, int)
import           Formatting.Time (monthNameShort, dayOfMonth)
import           Graphics.Vty hiding ((<|>), update, text)
import           Graphics.Vty.Widgets.All hiding (wrap)
import qualified Network.Wreq as Wreq
import           Network.Wreq hiding (Proxy, get, put, header)
import           System.Exit (exitSuccess)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (readProcess)
import qualified Text.Atom.Feed as Atom
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types

import           LambdaFeed.Widgets
import           LambdaFeed.Types

hn :: String
hn = "https://news.ycombinator.com/rss"

runningmusic :: String
runningmusic = "http://www.reddit.com/r/runningmusic/.rss"

nullprogram :: String
nullprogram = "http://nullprogram.com/feed/"

feedsToFetch :: [String]
feedsToFetch = [hn
               ,runningmusic
               ,nullprogram
               ,"http://lifehacker.com/index.xml"
               ,"http://www.reddit.com/r/haskell/.rss"
               ]

catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

explode :: UTCTime -> Feed -> Seq FeedItem
explode now f = fmap (convertFeedItem now f) (Seq.fromList $ getFeedItems f)

convertFeedItem :: UTCTime -> Feed -> Item -> FeedItem
convertFeedItem now f i = FeedItem title url curl content pubDateOrNow chan
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        chan = Channel (T.pack (getFeedTitle f)) (T.pack <$> (getFeedHome f))
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now

fetchAll :: [String] -> IO (Seq FeedItem)
fetchAll urls = getCurrentTime >>= \t -> foldM (go t) Seq.empty urls
  where go now acc url = maybe acc (\feed -> acc >< (explode now feed)) <$> fetch url

try' :: IO a -> IO (Either SomeException a)
try' = try

fetch :: String -> IO (Maybe Feed)
fetch url = do
  feedOrErr <- try' (Wreq.get url)
  case feedOrErr of
    Left _ -> return Nothing
    Right feed -> return . parseFeedString . view (responseBody . utf8 . from packed) $ feed

addAll :: Foldable f => Widget (List FeedItem FormattedText) -> f FeedItem -> IO ()
addAll list set = for_ (zip [(1::Int)..] (toList set)) $ \(i,item) -> do
  let description = fromJust (view itemTitle item <|> view itemUrl item <|> Just "<unknown>")
      pdate = view itemPubDate item
      fmt = (left 4 ' ' %. int)
          % "  "
          % monthNameShort
          % " "
          % dayOfMonth
          % "   "
          % stext
  label <- plainText $ sformat fmt i pdate pdate description
  addToList list item label

updateChannelFromAcid :: Widget (List Channel FormattedText) -> AcidState Database -> IO ()
updateChannelFromAcid w acid = do
  (unreadItems,readItems) <- query' acid AllItems
  clearList w
  for_ (Map.keys unreadItems) $ \chan -> do
    let numItemsUnread = maybe 0 Seq.length $ Map.lookup chan unreadItems
        numItemsRead = maybe 0 Seq.length $ Map.lookup chan readItems
        total = numItemsRead + numItemsUnread
        fmtTotal = sformat ("(" % int % "/" % int % ")") numItemsUnread total
    lbl <- plainText $ sformat ((left 11 ' ' %. stext) % " " % stext)
                               fmtTotal (describeChannel chan)
    addToList w chan lbl

updateItemsFromAcid :: Channel
                    -> Widget (List FeedItem FormattedText)
                    -> AcidState Database
                    -> IO ()
updateItemsFromAcid k w acid = do
  items <- query' acid UnreadItems
  clearList w
  addAll w (items Map.! k)

wrap :: (Show a, Show b, Show c)
     => Widget a
     -> Widget b
     -> Widget c
     -> IO (Widget (Box (Box (Box a HFill) c) (Box b HFill)))
wrap header footer widget = do
  header' <- pure header <++> hFill ' ' 1
  footer' <- pure footer <++> hFill ' ' 1
  setNormalAttribute header' $ Attr KeepCurrent KeepCurrent (SetTo black)
  setNormalAttribute footer' $ Attr KeepCurrent KeepCurrent (SetTo black)
  pure header' <--> pure widget <--> pure footer'

renderItem :: FeedItem -> RenderedItem
renderItem i =
  RenderedItem feedTitle' itemTitle' itemLink' itemComments' pubDate' pandocResult
  where pandocRender = unsafePerformIO . readProcess "/usr/bin/pandoc" ["-f"
                                                                       ,"html"
                                                                       ,"-t"
                                                                       ,"markdown"
                                                                       ,"--reference-links"
                                                                       ]
        content = view (itemContent . non "Cannot render content.") i
        pandocResult = content & _Text %~ pandocRender
        feedTitle' = view (itemChannel . to describeChannel) $ i
        itemTitle' = view (itemTitle . non "<No title>") i
        itemLink' = view (itemUrl . non "<No link>") i
        itemComments' = view (itemCommentUrl . non "<No comments link>") i
        pubDate' = formatPubDate . view itemPubDate $ i
        formatPubDate :: UTCTime -> Text
        formatPubDate = T.pack . formatTime defaultTimeLocale rfc822DateFormat

display :: RenderedItem -> [(Text,Attr)]
display r = [("Feed: " <> view renderedFeed r, myHeaderHighlight)
            ,("Title: " <> view renderedItemTitle r, myHeaderHighlight)
            ,("Link: " <> view renderedUrl r, myHeaderHighlight)
            ,("Comments: " <> view renderedCommentUrl r, myHeaderHighlight)
            ,("Date: " <> view renderedPubDate r, myHeaderHighlight)
            ,("\n", myDefAttr)
            ,(view renderedContent r, myDefAttr)
            ]

newList' :: Show b => Int -> IO (Widget (List a b))
newList' i = do l <- newList i
                setSelectedFocusedAttr l (Just myDefHighlight)
                setNormalAttribute l myDefAttr
                return l

myDefHighlight :: Attr
myDefHighlight = black' `on` orange `withStyle` bold
  where black' = rgbColor (0 :: Int) 0 0

myDefAttr :: Attr
myDefAttr = defAttr `withForeColor` white

orange :: Color
orange = rgbColor 215 135 (0 :: Int)

myHeaderHighlight :: Attr
myHeaderHighlight = myDefAttr `withForeColor` orange `withStyle` bold

withAcid :: AcidState Database -> IO ()
withAcid acid = do
  header <- plainText "λ Feed"
  footer <- plainText ""
  channelList <- (newList' 1)
  channelList `onKeyPressed` viKeys
  updateChannelFromAcid channelList acid
  channelUI <- centered channelList >>= wrap header footer

  fgChannel <- newFocusGroup
  fgChannel `onKeyPressed` (channelKeyHandler channelList footer acid)
  void $ addToFocusGroup fgChannel channelList

  itemList <- (newList' 1)
  itemList `onKeyPressed` viKeys
  itemUI <- centered itemList >>= wrap header footer

  fgItems <- newFocusGroup
  fgItems `onKeyPressed` (channelKeyHandler channelList footer acid)
  void $ addToFocusGroup fgItems itemList

  contentWidget <- newArticleWidget
  contentUI <- wrap header footer contentWidget
  fgContent <- newFocusGroup
  void $ addToFocusGroup fgContent contentWidget

  c <- newCollection
  channelView <- addToCollection c channelUI fgChannel
  itemView <- addToCollection c itemUI fgItems
  contentView <- addToCollection c contentUI fgContent

  channelList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    clearList itemList
    updateItemsFromAcid item itemList acid
    itemView

  itemList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    setArticle contentWidget (display (renderItem item))
    contentView

  fgItems `onKeyPressed` \_ k _ -> case k of
    (KChar 's') -> channelView >> return True
    (KChar 'q') -> channelView >> return True
    _ -> return False

  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 's') -> itemView >> return True
    _ -> return False

  fgContent `onKeyPressed` \_ k _ -> case k of
    (KChar 'q') -> itemView >> return True
    _ -> return False

  runUi c defaultContext
  exitSuccess

channelKeyHandler :: Widget (List Channel FormattedText)
                  -> Widget FormattedText
                  -> AcidState Database
                  -> a
                  -> Key
                  -> b
                  -> IO Bool
channelKeyHandler _ _ _ _ (KChar 'Q') _= exitSuccess >> return True
channelKeyHandler w footer acid _ (KChar 'u') _= do
  setText footer "Updating..."
  void . forkIO $ do
    feeds <- fetchAll feedsToFetch
    update' acid (UpdateFeeds feeds)
    updateChannelFromAcid w acid
    schedule $ setText footer "Done."
  return True
channelKeyHandler _ _ _ _ _ _= return False

viKeys :: Widget (List a b) -> Key -> t -> IO Bool
viKeys = handler
  where handler w (KChar 'j') _ = scrollDown w >> return True
        handler w (KChar 'k') _ = scrollUp w >> return True
        handler w (KChar 'g') _ = scrollToBeginning w >> return True
        handler w (KChar 'G') _ = scrollToEnd w >> return True
        handler w (KChar ':') _ = setSelected w 5 >> return True
        handler _ _ _ = return False

main :: IO ()
main = bracket (openLocalState initialDb) createCheckpointAndClose withAcid

getFeedContent :: Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing
