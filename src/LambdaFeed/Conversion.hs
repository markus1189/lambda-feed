{-# LANGUAGE CPP #-}
module LambdaFeed.Conversion (convertFeedToFeedItems) where

import           Control.Applicative ((<|>))
import           Control.Lens (lazy, view, below, review,(#))
import           Control.Monad (join)
import           Data.Digest.Pure.SHA (sha1, showDigest)
#if __GLASGOW_HASKELL__ >= 710
#else
import           Data.Functor ((<$>))
#endif
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import qualified Text.Atom.Feed as Atom
import           Text.Feed.Query
import           Text.Feed.Types as Ext

import           LambdaFeed.Types

convertFeedToFeedItems :: Text -> UTCTime -> Ext.Feed -> Seq FeedItem
convertFeedToFeedItems src now extFeed = convertFeedItem src now extFeed
                                     <$> Seq.fromList (getFeedItems extFeed)

convertFeedItem :: Text -> UTCTime -> Ext.Feed -> Item -> FeedItem
convertFeedItem src now srcFeed i = FeedItem title url curl content pubDateOrNow chan guid
  where title = T.pack <$> getItemTitle i
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        guid = ((review _IdFromFeed . snd) <$> getItemId i)
           <|> (_IdFromLink #) <$> getItemLink i
           <|> sha
        chan = Channel (T.pack (getFeedTitle srcFeed)) (T.pack <$> getFeedHome srcFeed) src
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now
        sha = review (below _IdFromContentSHA) $
                showDigest . sha1 . view lazy . T.encodeUtf8 <$> (content <> title)

getFeedContent :: Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing
