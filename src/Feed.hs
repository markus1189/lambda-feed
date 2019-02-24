{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Feed where

import qualified Brick.BChan as Brick
import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Foldable (for_, traverse_)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Network.HTTP.Client (HttpException)
import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import as FeedImport
import qualified Text.Feed.Query as FeedQuery
import           Text.Feed.Types (Feed, Item)

import Events

downloadFeeds chan = do
  urls <- lines <$> readFile "urls"
  for_ urls $ \url -> do
    responseE <- E.try @HttpException (Wreq.get url)
    case responseE of
      Left e -> return () -- TODO: how to handle error?
      Right response -> do
        let body = response ^. Wreq.responseBody
            result = FeedImport.parseFeedSource body
            feed = maybe (Left ParseFailed) Right result
        traverse_ (Brick.writeBChan chan . DownloadedFeed) feed

getItemTitle' :: Item -> Text
getItemTitle' = fromMaybe "<no title>" . FeedQuery.getItemTitle

data DownloadError = ParseFailed
                   | HttpError HttpException
                   deriving Show

data DownloadResult = DownloadResult { _drUrl :: String
                                     , _drFeed :: Either DownloadError Feed
                                     } deriving Show
makeLenses ''DownloadResult
