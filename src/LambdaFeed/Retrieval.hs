{-# LANGUAGE TupleSections #-}
module LambdaFeed.Retrieval (fetchActor) where

import           Control.Exception.Base (try)
import           Control.Lens (view, from, strict)
import           Control.Monad (forever)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Time (getCurrentTime)
import           Network.HTTP.Client (HttpException)
import qualified Network.Wreq as Wreq
import           Pipes
import           Pipes.Concurrent
import           System.Timeout (timeout)
import           Text.Feed.Import (parseFeedString)

import           LambdaFeed.Actor
import           LambdaFeed.Conversion (convertFeedToFeedItems)
import           LambdaFeed.Types

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = try

fetch1 :: Int -> Text -> IO (Either RetrievalError (Seq FeedItem))
fetch1 dt url = do
  feedOrErr <- timeout dt $ tryHttp (Wreq.get (T.unpack url))
  case feedOrErr of
     Nothing -> return . Left $ TimeOutDuringRetrieve url dt
     Just (Left e) -> return . Left $ RetrievalHttpError url e
     Just (Right resp) -> do
       case parse resp of
          Nothing -> return . Left $
                       FeedParseError url (T.decodeUtf8 . view (Wreq.responseBody . strict) $ resp)
          Just f -> do
            currTime <- getCurrentTime
            return . Right $ convertFeedToFeedItems url currTime f
  where parse = parseFeedString . view (Wreq.responseBody . utf8 . from packed)

fetchActor :: Int -> IO (Actor FetcherControl FetcherEvent)
fetchActor dt = newActor unbounded unbounded $ forever $ do
  evt <- await
  case evt of
    StartFetch us -> do
      for (each us) $ \u -> (do
        startTime <- liftIO getCurrentTime
        yield (StartedSingleFetch startTime u)
        r <- liftIO $ fetch1 dt u
        case r of
          Left e -> yield (ErrorDuringFetch u e)
          Right items -> do
            now <- liftIO getCurrentTime
            yield (CompletedSingleFetch now u items))
      endTime <- liftIO getCurrentTime
      yield (FetchFinished endTime)
