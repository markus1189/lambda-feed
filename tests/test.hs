{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Foldable (toList, traverse_)
import           Control.Lens (view, uses)
import           Control.Lens.Operators
import           LambdaFeed.Types
import           Control.Monad.State
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Clock (UTCTime)

import           Test.Tasty
import           Test.Tasty.QuickCheck (Gen, choose, frequency, listOf, listOf1, elements, oneof)
import qualified Test.Tasty.QuickCheck as QC

maybeText :: Gen (Maybe Text)
maybeText = fmap T.pack <$> (frequency [(1,pure Nothing)
                                       ,(5,Just <$> listOf (elements ['A'..'z']))])

someText :: Gen Text
someText = T.pack <$> listOf (elements ['A'..'z'])

utcTime :: Gen UTCTime
utcTime = posixSecondsToUTCTime . fromInteger <$> choose (0,1433176781)

itemIdGen :: FeedItem -> Gen (Maybe ItemId)
itemIdGen fi = oneof [(Just . IdFromFeed <$> listOf (elements ['A'..'z']))
                     ,pure (guidOrSHA fi)
                     ]

feedItem :: Channel -> Gen FeedItem
feedItem chan = do
  fi <- FeedItem <$> maybeText
                 <*> maybeText
                 <*> maybeText
                 <*> maybeText
                 <*> utcTime
                 <*> pure chan
                 <*> pure Nothing
  guid <- itemIdGen fi
  return (fi & itemId .~ guid)

dropHead :: Seq a -> [Seq a]
dropHead s = if Seq.null s then [] else [Seq.drop 1 s]

channel :: Gen Channel
channel = Channel <$> someText <*> maybeText <*> someText

sampleData :: Gen (Seq FeedItem)
sampleData = do
  chans <- listOf channel
  items <- traverse (listOf1 . feedItem) chans
  return $ Seq.fromList (concat items)

-- shrinkSampleData :: Seq FeedItem -> [Seq FeedItem]
-- shrinkSampleData = map Seq.fromList . filterM (const [True,False]) . toList

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [specTests]

withInitDb :: State Database a -> Database
withInitDb = flip execState initialDb

getChannels :: Foldable f => f FeedItem -> Set Channel
getChannels = Set.fromList . map (view itemChannel) . toList

markAllRead :: Foldable f => f FeedItem -> State Database ()
markAllRead = traverse_ nonAcidMarkAsRead . getChannels

specTests :: TestTree
specTests = testGroup "QuickCheck"
  [QC.testProperty "update is idempotent" $
     QC.forAllShrink sampleData dropHead $ \feeds ->
       withInitDb (nonAcidUpdateFeeds feeds) ==
         withInitDb (replicateM 2 (nonAcidUpdateFeeds feeds))
  ,QC.testProperty "update is idempotent wrt seenItems with mark read in between" $
     QC.forAllShrink sampleData dropHead $ \feeds ->
       (withInitDb (nonAcidUpdateFeeds feeds)) ^. seenItems ==
          withInitDb (nonAcidUpdateFeeds feeds
                   >> markAllRead feeds
                   >> nonAcidUpdateFeeds feeds) ^. seenItems
  ,QC.testProperty "mark all read clears all unread items" $
    QC.forAllShrink sampleData dropHead $ \feeds ->
      flip evalState initialDb (nonAcidUpdateFeeds feeds
                             >> markAllRead feeds
                             >> uses unreadFeeds Map.null)
  ]
