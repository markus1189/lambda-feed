{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import           LambdaFeed.Retrieval
import           LambdaFeed.Types
import           Control.Monad.State
import           Control.Monad ((=<<))
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

import           Test.Tasty
import           Test.Tasty.SmallCheck as SC
import           Test.Tasty.HUnit
import           Test.Tasty.Hspec

main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence [specTests]

withInitDb :: State Database a -> Database
withInitDb = flip execState initialDb

fetchFeeds :: IO (Seq FeedItem)
fetchFeeds = do
  Right feeds <- fetch1 (30 * 1000 * 1000) "http://www.reddit.com/r/haskell/.rss"
  return feeds

specTests :: IO TestTree
specTests = testGroup "Specs" <$>
  sequence
    [testSpec "HSpec" $
         it "update is idempotent if feeds are fixed" $ do
           feeds <- fetchFeeds
           withInitDb (nonAcidUpdateFeeds feeds) `shouldBe`
             withInitDb (replicateM 2 (nonAcidUpdateFeeds feeds))
      ]
