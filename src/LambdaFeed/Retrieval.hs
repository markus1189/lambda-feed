{-# LANGUAGE TupleSections #-}
module LambdaFeed.Retrieval (fetchActor, fetch1) where

import           Control.Exception.Base (try,catch)
import           Control.Lens (view, strict)
import           Control.Monad (forever)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
     Just (Right resp) ->
       case parse resp of
          Nothing -> return . Left $ FeedParseError url
          Just f -> do
            currTime <- getCurrentTime
            return . Right $ convertFeedToFeedItems url currTime f
  where parse = parseFeedString . T.unpack . T.decodeUtf8 . view (Wreq.responseBody . strict)

fetchActor :: Int -> IO (Actor FetcherControl FetcherEvent)
fetchActor dt = newActor (newest 1) unbounded $ forever $ do
  evt <- await
  case evt of
    StartFetch us -> do
      let total = length us
      for (each (zip [1..] us)) $ \(i,u) -> (do
        startTime <- liftIO getCurrentTime
        yield (StartedSingleFetch startTime u (i,total))
        r <- liftIO $ fetch1 dt u `catch` (return . Left . UncaughtException u)
        case r of
          Left e -> yield (ErrorDuringFetch u e)
          Right items -> do
            now <- liftIO getCurrentTime
            yield (CompletedSingleFetch now u items))
      endTime <- liftIO getCurrentTime
      yield (FetchFinished endTime)
