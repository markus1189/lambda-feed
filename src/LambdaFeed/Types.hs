{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module LambdaFeed.Types (Channel(Channel)
                        ,channelTitle
                        ,channelUrl
                        ,describeChannel

                        ,FeedItem(FeedItem)
                        ,feedTitle
                        ,feedUrl
                        ,feedContent
                        ,feedChannel

                        ,RssDb(RssDb)
                        ,readFeeds
                        ,unreadFeeds
                        ,initialDb
                        ,markItemAsRead

                        ,QueryItems(..)
                        ,UpdateFeeds(..)
                        ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens (view, use, lazy, _2, at, ix)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Acid
import           Data.Data (Data, Typeable)
import           Data.Digest.Pure.SHA
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.SafeCopy
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

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

data RssDb = RssDb { _unreadFeeds :: Map Channel (Seq FeedItem)
                   , _readFeeds :: Map Channel (Seq FeedItem)
                   , _seenItems :: Set String
                   } deriving (Data,Typeable)
$(deriveSafeCopy 1 'base ''RssDb)
makeLenses ''RssDb

queryItems :: Query RssDb (Map Channel (Seq FeedItem))
queryItems = view unreadFeeds

markItemAsRead :: FeedItem -> Update RssDb ()
markItemAsRead i = do
  unreadFeeds %= (at (view feedChannel i) .~ Nothing)
  readFeeds %= (ix (view feedChannel i) %~ \is -> is Seq.>< (Seq.singleton i))

updateFeeds :: Seq FeedItem -> Update RssDb ()
updateFeeds feeds = do
  seen <- use seenItems
  unreadFeeds %= \uf -> Map.unionWith (><) uf (buildStoreWithNew seen feeds)
  seenItems %= Set.union (computeSHAs feeds)

buildStoreWithNew :: (Functor f, Foldable f) => Set String -> f FeedItem -> Map Channel (Seq FeedItem)
buildStoreWithNew seen = Map.fromListWith (><)
                       . map (_2 %~ Seq.filter (\item ->
                           case showDigest <$> (itemSHA item) of
                             Nothing -> False
                             Just hash -> not (Set.member hash seen)))
                       . toList
                       . fmap (view feedChannel &&& Seq.singleton)

computeSHAs :: Foldable f => f FeedItem -> Set String
computeSHAs = foldl' go Set.empty
  where go acc (itemSHA -> Just hash) = Set.insert (showDigest hash) acc
        go acc _ = acc

itemSHA :: FeedItem -> Maybe (Digest SHA1State)
itemSHA i = sha1 . view lazy . T.encodeUtf8 <$> (maybeItemContent <> maybeChannelTitle)
  where maybeItemContent = view feedContent i
        maybeChannelTitle = view (feedChannel . channelUrl) i

$(makeAcidic ''RssDb ['queryItems, 'updateFeeds])

initialDb :: RssDb
initialDb = RssDb Map.empty Map.empty Set.empty
