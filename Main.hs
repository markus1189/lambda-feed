{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent (forkIO)
import           Control.Exception (bracket, SomeException, try)
import           Control.Lens (from, view, use, lazy, _2, non, at, ix)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Advanced (update', query')
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.Data (Data, Typeable)
import           Data.Digest.Pure.SHA
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import           Data.Monoid ((<>))
import           Data.SafeCopy
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens hiding (text)
import qualified Data.Text.Strict.Lens as LS
import           Formatting (sformat, left, (%), stext)
import           Graphics.Vty hiding ((<|>), update, text)
import           Graphics.Vty.Widgets.All hiding (wrap)
import qualified Network.Wreq as Wreq
import           Network.Wreq hiding (Proxy, get, put, header)
import           System.Exit (exitSuccess)
import           System.Process (readProcess)
import qualified Text.Atom.Feed as Atom
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types

data Channel = Channel { _channelTitle :: Text
                       , _channelUrl :: Maybe Text
                       } deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Channel
$(deriveSafeCopy 0 'base ''Channel)

describeChannel :: Channel -> Text
describeChannel (Channel title Nothing) = title
describeChannel (Channel title (Just url)) = url <> ":  " <> title

data FeedItem = FeedItem { _feedTitle :: Maybe Text
                         , _feedUrl :: Maybe Text
                         , _feedContent :: Maybe Text
                         , _feedChannel :: Channel
                         } deriving (Show, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeedItem)
makeLenses ''FeedItem

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
               ]

itemSHA :: FeedItem -> Maybe (Digest SHA1State)
itemSHA i = sha1 . view lazy . T.encodeUtf8 <$> (maybeItemContent <> maybeChannelTitle)
  where maybeItemContent = view feedContent i
        maybeChannelTitle = view (feedChannel . channelUrl) i

buildStoreWithNew :: (Functor f, Foldable f) => Set String -> f FeedItem -> Map Channel (Seq FeedItem)
buildStoreWithNew seen = Map.fromListWith (><)
                       . map (_2 %~ Seq.filter (\item ->
                           case showDigest <$> (itemSHA item) of
                             Nothing -> False
                             Just hash -> not (Set.member hash seen)))
                       . toList
                       . fmap (view feedChannel &&& Seq.singleton)

data RssDb = RssDb { _unreadFeeds :: Map Channel (Seq FeedItem)
                   , _readFeeds :: Map Channel (Seq FeedItem)
                   , _seenItems :: Set String
                   } deriving (Data,Typeable)
$(deriveSafeCopy 1 'base ''RssDb)
makeLenses ''RssDb

queryItems :: Query RssDb (Map Channel (Seq FeedItem))
queryItems = view unreadFeeds

computeSHAs :: Foldable f => f FeedItem -> Set String
computeSHAs = foldl' go Set.empty
  where go acc (itemSHA -> Just hash) = Set.insert (showDigest hash) acc
        go acc _ = acc

markItemAsRead :: FeedItem -> Update RssDb ()
markItemAsRead i = do
  unreadFeeds %= (at (view feedChannel i) .~ Nothing)
  readFeeds %= (ix (view feedChannel i) %~ \is -> is Seq.>< (Seq.singleton i))

updateFeeds :: Seq FeedItem -> Update RssDb ()
updateFeeds feeds = do
  seen <- use seenItems
  unreadFeeds %= \uf -> Map.unionWith (><) uf (buildStoreWithNew seen feeds)
  seenItems %= Set.union (computeSHAs feeds)

$(makeAcidic ''RssDb ['queryItems, 'updateFeeds])

initialDb :: RssDb
initialDb = RssDb Map.empty Map.empty Set.empty

catMaybesSeq :: Seq (Maybe a) -> Seq a
catMaybesSeq = fmap fromJust . Seq.filter isJust

explode :: Feed -> Seq FeedItem
explode f = fmap (convertFeedItem f) (Seq.fromList $ getFeedItems f)

convertFeedItem :: Feed -> Item -> FeedItem
convertFeedItem f i = FeedItem title url content chan
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        content = getFeedContent i
        chan = Channel (T.pack (getFeedTitle f)) (T.pack <$> (getFeedHome f))

fetchAll :: [String] -> IO (Seq FeedItem)
fetchAll urls = foldM go Seq.empty urls
  where go acc url = maybe acc (\feed -> acc >< (explode feed)) <$> fetch url

try' :: IO a -> IO (Either SomeException a)
try' = try

fetch :: String -> IO (Maybe Feed)
fetch url = do
  feedOrErr <- try' (Wreq.get url)
  case feedOrErr of
    Left _ -> return Nothing
    Right feed -> return . parseFeedString . view (responseBody . utf8 . from packed) $ feed

itemTitle :: Item -> Text
itemTitle item = maybe ("<no title given>") (view LS.packed) $ getItemTitle $ item

addAll :: Foldable f => Widget (List FeedItem FormattedText) -> f FeedItem -> IO ()
addAll list set = for_ set $ \x -> do
  pt <- plainText (fromJust $ view feedTitle x <|> view feedUrl x <|> Just "<unknown>")
  addToList list x pt

updateChannelFromAcid :: Widget (List Channel FormattedText) -> AcidState RssDb -> IO ()
updateChannelFromAcid w acid = do
  items <- query' acid QueryItems
  clearList w
  for_ (Map.keys items) $ \chan -> do
    let numItems = Seq.length $ items Map.! chan
    lbl <- plainText $ sformat ("(" % left 4 ' ' % ") " % stext) numItems (describeChannel chan)
    addToList w chan lbl

updateItemsFromAcid :: Channel
                    -> Widget (List FeedItem FormattedText)
                    -> AcidState RssDb
                    -> IO ()
updateItemsFromAcid k w acid = do
  items <- query' acid QueryItems
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

pandocRender :: Text -> IO Text
pandocRender s = T.pack <$> (readProcess "/usr/bin/pandoc" ["-f", "html", "-t", "markdown", "--reference-links"] . T.unpack $ s)

newList' :: Show b => Int -> IO (Widget (List a b))
newList' i = do l <- newList i
                setSelectedFocusedAttr l (Just $ black' `on` orange)
                return l
  where black' = rgbColor (0 :: Int) 0 0
        orange = rgbColor 215 135 (0 :: Int)

withAcid :: AcidState RssDb -> IO ()
withAcid acid = do
  header <- plainText "LambaFeed"
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

  contentWidget <- plainText ""
  contentWidget' <- pure contentWidget <--> vFill ' '
  contentUI <- wrap header footer contentWidget'
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

  itemList `onItemActivated` \(ActivateItemEvent _ entry _) -> do
    rendered <- pandocRender (view (feedContent . non "<no content found or invalid>") $ entry)
    setText contentWidget rendered
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
                  -> AcidState RssDb
                  -> a
                  -> Key
                  -> b
                  -> IO Bool
channelKeyHandler _ _ _ _ (KChar 'q') _= exitSuccess >> return True
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
        handler _ _ _ = return False

main :: IO ()
main = bracket (openLocalState initialDb) createCheckpointAndClose withAcid

getFeedContent :: Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing
