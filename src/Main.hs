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
module Main (main) where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Exception (bracket, SomeException, try)
import           Control.Lens (from, view, review, lazy, below)
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Advanced (update', query')
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.Digest.Pure.SHA
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Time (getCurrentTime, UTCTime)
import           Formatting (sformat, left, (%), (%.), stext, int)
import           Graphics.Vty (Attr(Attr), MaybeDefault(KeepCurrent,SetTo), black, Key(KChar))
import           Graphics.Vty.Widgets.All hiding (wrap)
import qualified Network.Wreq as Wreq
import           Network.Wreq hiding (Proxy, get, put, header)
import           Pipes.Concurrent (send, spawn', bounded, atomically)
import qualified Text.Atom.Feed as Atom
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types

import           LambdaFeed
import           LambdaFeed.Types
import           LambdaFeed.Widgets

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

explode :: UTCTime -> Feed -> Seq FeedItem
explode now f = fmap (convertFeedItem now f) (Seq.fromList $ getFeedItems f)

convertFeedItem :: UTCTime -> Feed -> Item -> FeedItem
convertFeedItem now f i = FeedItem title url curl content pubDateOrNow chan guid
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        guid = (review _IdFromFeed . snd) <$> getItemId i <|> sha
        chan = Channel (T.pack (getFeedTitle f)) (T.pack <$> (getFeedHome f))
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now
        sha = review (below _IdFromContentSHA) $ showDigest . sha1 . view lazy . T.encodeUtf8 <$> (content <> title)

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
updateChannelFromAcid :: Widget (List Channel FormattedText) -> AcidState Database -> IO ()
updateChannelFromAcid w acid = do
  (unreadItems,readItems) <- query' acid AllItems
  saveSelection w $ do
    clearList w
    for_ (Map.keys unreadItems) $ \chan -> do
      let numItemsUnread = maybe 0 Seq.length $ Map.lookup chan unreadItems
          numItemsRead = maybe 0 Seq.length $ Map.lookup chan readItems
          total = numItemsRead + numItemsUnread
          fmtTotal = sformat ("(" % int % "/" % int % ")") numItemsUnread total
      lbl <- plainText $ sformat ((left 11 ' ' %. stext) % " " % stext)
                                 fmtTotal (view channelTitle chan)
      addToList w chan lbl

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

newList' :: Show b => Int -> IO (Widget (List a b))
newList' i = do l <- newList i
                setSelectedFocusedAttr l (Just myDefHighlight)
                setNormalAttribute l myDefAttr
                return l

setupGui :: (GuiEvent -> IO Bool)
         -> AcidState Database
         -> IO (LFCfg, LFState, Collection)
setupGui trigger acid = do
  header <- plainText "λ Feed"
  footer <- plainText ""

  channelList <- newList' 1
  channelList `onKeyPressed` viKeys
  updateChannelFromAcid channelList acid
  channelUI <- centered channelList >>= wrap header footer

  fgChannel <- newFocusGroup
  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 'u') -> do
      setText footer "Updating..."
      void . forkIO $ do
        feeds <- fetchAll feedsToFetch
        update' acid (UpdateFeeds feeds)
        updateChannelFromAcid channelList acid
        schedule $ setText footer "Done."
      return True
    _ -> return False

  void $ addToFocusGroup fgChannel channelList

  itemList <- newList' 1
  itemList `onKeyPressed` viKeys
  itemUI <- centered itemList >>= wrap header footer

  fgItems <- newFocusGroup
  void $ addToFocusGroup fgItems itemList

  contentWidget' <- newArticleWidget
  contentUI <- wrap header footer contentWidget'
  fgContent <- newFocusGroup
  void $ addToFocusGroup fgContent contentWidget'

  c <- newCollection
  channelView <- addToCollection c channelUI fgChannel
  itemView <- addToCollection c itemUI fgItems
  contentView <- addToCollection c contentUI fgContent

  fgItems `onKeyPressed` \_ k _ -> case k of
    (KChar 'q') -> trigger BackToChannels
    _ -> return False

  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 'Q') -> trigger QuitLambdaFeed
    _ -> return False

  fgContent `onKeyPressed` \_ k _ -> case k of
    (KChar 'q') -> trigger BackToItems
    _ -> return False

  channelList `onItemActivated` \(ActivateItemEvent _ chan _) -> do
    void $ trigger (ChannelActivated chan)

  itemList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    void $ trigger (ItemActivated item)

  let cfg = LFCfg acid switches widgets
      switches = SwitchTo channelView itemView contentView
      widgets = LFWidgets channelList itemList contentWidget'
  return (cfg,initialLFState,c)

viKeys :: Widget (List a b) -> Key -> t -> IO Bool
viKeys = handler
  where handler w (KChar 'j') _ = scrollDown w >> return True
        handler w (KChar 'k') _ = scrollUp w >> return True
        handler w (KChar 'g') _ = scrollToBeginning w >> return True
        handler w (KChar 'G') _ = scrollToEnd w >> return True
        handler w (KChar ':') _ = setSelected w 5 >> return True
        handler _ _ _ = return False

main :: IO ()
main =
  bracket (openLocalState initialDb) createCheckpointAndClose $ \acid -> do
  (output,input,seal) <- spawn' (bounded 1)
  (cfg,s,c) <- setupGui (\e -> atomically $ send output e) acid
  start seal input cfg s
  runUi c defaultContext

getFeedContent :: Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing
