module LambdaFeed.Retrieval (fetch1, fetchN, RetrievalError(..)) where

import           Control.Exception.Extra (try_)
import           Control.Lens (view, from)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Time (getCurrentTime)
import qualified Network.Wreq as Wreq
import           Text.Feed.Import (parseFeedString)

import           LambdaFeed.Conversion (convertFeedToFeedItems)
import           LambdaFeed.Types (FeedItem)

data RetrievalError = ConnectionError Text | FeedParseError Text

fetch1 :: Text -> IO (Either RetrievalError (Seq FeedItem))
fetch1 url = do
  feedOrErr <- try_ (Wreq.get (T.unpack url))
  (case feedOrErr of
     Left _ -> return . Left $ ConnectionError url
     Right feed -> do
       (case parse feed of
          Nothing -> return . Left $ FeedParseError url
          Just f -> do
            currTime <- getCurrentTime
            return . Right $ convertFeedToFeedItems currTime f))
  where parse = parseFeedString . view (Wreq.responseBody . utf8 . from packed)

fetchN :: Traversable f => f Text -> IO (f (Either RetrievalError (Seq FeedItem)))
fetchN = traverse fetch1
