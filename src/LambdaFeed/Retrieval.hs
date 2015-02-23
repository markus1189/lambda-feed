{-# LANGUAGE TupleSections #-}
module LambdaFeed.Retrieval (fetch1, fetchP) where

import           Control.Exception.Extra (try_)
import           Control.Lens (view, from, strict)
import           Data.Functor ((<$>))
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Time (getCurrentTime)
import qualified Network.Wreq as Wreq
import           Pipes
import qualified Pipes.Prelude as P
import           Text.Feed.Import (parseFeedString)

import           LambdaFeed.Conversion (convertFeedToFeedItems)
import           LambdaFeed.Types (FeedItem,RetrievalError(..))

fetch1 :: Text -> IO (Either RetrievalError (Seq FeedItem))
fetch1 url = do
  feedOrErr <- try_ (Wreq.get (T.unpack url))
  case feedOrErr of
     Left e -> return . Left $ ConnectionError url e
     Right resp -> do
       case parse resp of
          Nothing -> return . Left $
                       FeedParseError url (T.decodeUtf8 . view (Wreq.responseBody . strict) $ resp)
          Just f -> do
            currTime <- getCurrentTime
            return . Right $ convertFeedToFeedItems currTime f
  where parse = parseFeedString . view (Wreq.responseBody . utf8 . from packed)

fetchP :: (Monad m, MonadIO m, Foldable f)
       => f Text
       -> Producer (Either RetrievalError (Text,(Seq FeedItem))) m ()
fetchP ts = each ts >-> P.mapM (\u -> fmap (u,) <$> liftIO (fetch1 u))
