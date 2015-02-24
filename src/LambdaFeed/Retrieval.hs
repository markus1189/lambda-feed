{-# LANGUAGE TupleSections #-}
module LambdaFeed.Retrieval (fetchP) where

import           Control.Exception.Base (try)
import           Control.Lens (view, from, strict)
import           Data.Functor ((<$>))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Time (getCurrentTime)
import           Network.HTTP.Client (HttpException)
import qualified Network.Wreq as Wreq
import           Pipes
import qualified Pipes.Prelude as P
import           Text.Feed.Import (parseFeedString)


import           LambdaFeed.Conversion (convertFeedToFeedItems)
import           LambdaFeed.Types (FeedItem,RetrievalError(..))
import           System.Timeout (timeout)

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
            return . Right $ convertFeedToFeedItems currTime f
  where parse = parseFeedString . view (Wreq.responseBody . utf8 . from packed)

fetchP :: (Monad m, MonadIO m, Foldable f)
       => Int
       -> f Text
       -> Producer (Either RetrievalError (Text,(Seq FeedItem))) m ()
fetchP dt ts = each ts >-> P.mapM (\u -> fmap (u,) <$> liftIO (fetch1 dt u))
