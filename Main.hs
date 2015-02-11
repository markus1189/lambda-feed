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
import           Control.Exception (bracket)
import           Control.Lens (from, view, strict)
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Acid.Advanced (update', query')
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.ByteString.Lens
import           Data.Data (Data, Typeable)
import           Data.Foldable
import           Data.Functor ((<$>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, catMaybes, fromJust)
import           Data.Proxy
import           Data.SafeCopy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy.Lens
import qualified Data.Text.Strict.Lens as LS
import           Data.Traversable
import           Graphics.Vty hiding ((<|>), update)
import           Graphics.Vty.Widgets.All
import qualified Network.Wreq as Wreq
import           Network.Wreq hiding (Proxy, get, put)
import           System.Exit (exitSuccess)
import qualified System.IO.UTF8 as U
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types (Feed, Item)

hn = "https://news.ycombinator.com/rss"
runningmusic = "http://www.reddit.com/r/runningmusic/.rss"

feedsToFetch = [hn, runningmusic, "http://nullprogram.com/feed/"]

data FeedItem = FeedItem { _title :: Maybe Text
                         , _url :: Maybe Text
                         , _content :: Maybe Text
                         , _feedTitle :: Text
                         , _feedUrl :: Text
                         } deriving (Show, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeedItem)

explode :: Feed -> [FeedItem]
explode f = catMaybes $ map (convertFeedItem f) (getFeedItems f)

convertFeedItem :: Feed -> Item -> Maybe FeedItem
convertFeedItem f i = FeedItem title url content feedTitle <$> feedUrl
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        content = T.pack <$> getItemDescription i
        feedTitle = T.pack (getFeedTitle f)
        feedUrl = T.pack <$> (getFeedHome f)

queryItems :: Query RssDb (FeedStore)
queryItems = asks _dbFeeds

fromFeeds :: (Functor f, Foldable f) => f FeedItem -> FeedStore
fromFeeds = Map.fromListWith (Set.union) . toList . fmap (\f -> (_feedUrl f, Set.singleton f))

updateFeeds :: [FeedItem] -> Update RssDb ()
updateFeeds feeds = do
  current <- gets _dbFeeds
  put . RssDb $ Map.unionWith Set.union current (fromFeeds feeds)

type FeedStore = Map Text (Set FeedItem)
data RssDb = RssDb { _dbFeeds :: FeedStore } deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''RssDb)
$(makeAcidic ''RssDb ['queryItems, 'updateFeeds])

initialDb = RssDb Map.empty

fetchAll :: [String] -> IO [FeedItem]
fetchAll urls = fmap (concatMap explode . catMaybes) $ traverse fetch urls

fetch :: String -> IO (Maybe Feed)
fetch url = parseFeedString
        <$> view (responseBody . utf8 . from packed)
        <$> Wreq.get url

itemTitle :: Item -> Text
itemTitle item = maybe ("<no title given>") (view LS.packed) $ getItemTitle $ item

addAll :: Foldable f => Widget (List FeedItem FormattedText) -> f FeedItem -> IO ()
addAll list set = for_ set $ \x -> do
  pt <- plainText (fromJust $ _title x <|> _url x <|> Just "<unknown>")
  addToList list x pt

updateChannelFromAcid :: Widget (List Text FormattedText) -> AcidState RssDb -> IO ()
updateChannelFromAcid w acid = do
  items <- query' acid QueryItems
  clearList w
--  addAll w (Set.unions $ toList items)
  for_ (Map.keys items) $ \i -> plainText i >>= addToList w i

updateItemsFromAcid :: Text -> Widget (List FeedItem FormattedText) -> AcidState RssDb -> IO ()
updateItemsFromAcid k w acid = do
  items <- query' acid QueryItems
  clearList w
  addAll w (items Map.! k)

withAcid :: AcidState RssDb -> IO ()
withAcid acid = do
  channelList <- (newList 1) :: IO (Widget (List Text FormattedText))
  channelList `onKeyPressed` viKeys
  updateChannelFromAcid channelList acid
  channelUI <- centered channelList

  fgChannel <- newFocusGroup
  fgChannel `onKeyPressed` (keyHandler channelList acid)
  addToFocusGroup fgChannel channelList

  itemList <- (newList 1) :: IO (Widget (List FeedItem FormattedText))
  itemList `onKeyPressed` viKeys
  itemUI <- centered itemList

  fgItems <- newFocusGroup
  fgItems `onKeyPressed` (keyHandler channelList acid)
  addToFocusGroup fgItems itemList

  c <- newCollection
  channelView <- addToCollection c channelUI fgChannel
  itemView <- addToCollection c itemUI fgItems

  channelList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    clearList itemList
    updateItemsFromAcid item itemList acid
    itemView

  fgItems `onKeyPressed` \_ k _ -> case k of
    (KChar 's') -> channelView >> return True
    (KChar 'q') -> channelView >> return True
    _ -> return False

  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 's') -> itemView >> return True
    _ -> return False

  runUi c defaultContext
  exitSuccess

keyHandler :: Widget (List Text FormattedText)
           -> AcidState RssDb
           -> a
           -> Key
           -> b
           -> IO Bool
keyHandler _ acid _ (KChar 'q') _= exitSuccess >> return True
keyHandler w acid _ (KChar 'u') _= do
  feeds <- fmap (concat . catMaybes) $ for feedsToFetch $ \f -> fmap explode <$> fetch f
  update' acid (UpdateFeeds feeds)
  updateChannelFromAcid w acid
  return True
keyHandler _ _ _ _ _= return False

viKeys = handler
  where handler w (KChar 'j') _ = scrollDown w >> return True
        handler w (KChar 'k') _ = scrollUp w >> return True
        handler _ _ _ = return False

main = bracket (openLocalState initialDb) createCheckpointAndClose withAcid
