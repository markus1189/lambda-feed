{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module LambdaFeed.Types (Channel(Channel)
                        ,channelTitle
                        ,channelUrl

                        ,FeedItem(FeedItem)
                        ,itemTitle
                        ,itemUrl
                        ,itemCommentUrl
                        ,itemContent
                        ,itemChannel
                        ,itemPubDate
                        ,itemId

                        ,ItemId
                        ,_IdFromFeed
                        ,_IdFromContentSHA

                        ,RenderedItem(RenderedItem)
                        ,renderedFeed
                        ,renderedItemTitle
                        ,renderedUrl
                        ,renderedCommentUrl
                        ,renderedPubDate
                        ,renderedContent
                        ,renderedId

                        ,Database(Database)
                        ,readFeeds
                        ,unreadFeeds
                        ,initialDb

                        ,AllItems(..)
                        ,UnreadItems(..)
                        ,UpdateFeeds(..)

                        ,LF
                        ,runLF

                        ,LFCfg(LFCfg)
                        ,lfAcid
                        ,lfSwitch
                        ,lfWidgets

                        ,SwitchTo(SwitchTo)
                        ,switchToChannels
                        ,switchToItems
                        ,switchToContent

                        ,LFWidgets(LFWidgets)
                        ,channelWidget
                        ,itemWidget
                        ,contentWidget

                        ,LFState
                        ,initialLFState

                        ,GuiEvent(..)
                        ) where

import           Control.Applicative
import           Control.Lens (view, use, lazy, at, non, review)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Data.Acid
import           Data.Data (Data, Typeable)
import           Data.Digest.Pure.SHA
import           Data.Foldable
import           Data.Function (on)
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
import           Data.Time
import           Graphics.Vty.Widgets.All (Widget, List, FormattedText)

data Channel = Channel { _channelTitle :: Text
                       , _channelUrl :: Maybe Text
                       } deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Channel
$(deriveSafeCopy 0 'base ''Channel)

data ItemId = IdFromFeed String | IdFromContentSHA String
  deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy 0 'base ''ItemId)
makePrisms ''ItemId

data FeedItem = FeedItem { _itemTitle :: Maybe Text
                         , _itemUrl :: Maybe Text
                         , _itemCommentUrl :: Maybe Text
                         , _itemContent :: Maybe Text
                         , _itemPubDate :: UTCTime
                         , _itemChannel :: Channel
                         , _itemId :: Maybe ItemId
                         } deriving (Show, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeedItem)
makeLenses ''FeedItem

data RenderedItem = RenderedItem { _renderedFeed :: Text
                                 , _renderedItemTitle :: Text
                                 , _renderedUrl :: Text
                                 , _renderedCommentUrl :: Text
                                 , _renderedPubDate :: Text
                                 , _renderedContent :: Text
                                 , _renderedId :: Text
                                 } deriving (Eq,Show)
makeLenses ''RenderedItem

data Database = Database { _unreadFeeds :: Map Channel (Seq FeedItem)
                         , _readFeeds :: Map Channel (Seq FeedItem)
                         , _seenItems :: Set ItemId
                         } deriving (Data,Typeable)
$(deriveSafeCopy 1 'base ''Database)
makeLenses ''Database

unreadItems :: Query Database (Map Channel (Seq FeedItem))
unreadItems = view unreadFeeds

allItems :: Query Database (Map Channel (Seq FeedItem), Map Channel (Seq FeedItem))
allItems = (,) <$> view unreadFeeds <*> view readFeeds

updateFeeds :: Seq FeedItem -> Update Database ()
updateFeeds feeds = do
  seen <- use seenItems
  unreadFeeds %= fmap (Seq.sortBy (\i1 i2 -> (compare `on` view itemPubDate) i2 i1))
               . Map.unionWith (><) (collectNewItems (`Set.member` seen) feeds)
  seenItems %= Set.union (generateIdentifiers feeds)

collectNewItems :: (Functor f, Foldable f) => (ItemId -> Bool) -> f FeedItem -> Map Channel (Seq FeedItem)
collectNewItems isKnown = foldl' step Map.empty
  where isNew (guidOrSHA -> (Just guid)) = not (isKnown guid)
        isNew _ = False

        step :: Map Channel (Seq FeedItem) -> FeedItem -> Map Channel (Seq FeedItem)
        step acc item = if isNew item
                           then acc & at (view itemChannel item) . non Seq.empty
                                    %~ (|> item)
                           else acc

generateIdentifiers :: Foldable f => f FeedItem -> Set ItemId
generateIdentifiers = foldl' go Set.empty
  where go acc (guidOrSHA -> Just guid) = Set.insert guid acc
        go acc _ = acc

guidOrSHA :: FeedItem -> Maybe ItemId
guidOrSHA i = view itemId i <|> (review _IdFromContentSHA . showDigest) <$> sha
  where sha = sha1 . view lazy . T.encodeUtf8 <$> (maybeItemContent <> maybeChannelTitle)
        maybeItemContent = view itemContent i
        maybeChannelTitle = view (itemChannel . channelUrl) i

$(makeAcidic ''Database ['unreadItems, 'allItems, 'updateFeeds])

initialDb :: Database
initialDb = Database Map.empty Map.empty Set.empty

data LFState = LFState
makeLenses ''LFState

initialLFState :: LFState
initialLFState = LFState

data SwitchTo = SwitchTo { _switchToChannels :: IO ()
                         , _switchToItems :: IO ()
                         , _switchToContent :: IO ()
                         }
makeLenses ''SwitchTo

data LFWidgets = LFWidgets { _channelWidget :: Widget (List Channel FormattedText)
                           , _itemWidget :: Widget (List FeedItem FormattedText)
                           , _contentWidget :: Widget (List Text FormattedText)
                           }
makeLenses ''LFWidgets

data LFCfg = LFCfg { _lfAcid :: AcidState Database
                   , _lfSwitch :: SwitchTo
                   , _lfWidgets :: LFWidgets
                   }
makeLenses ''LFCfg

newtype LF a = LF (ReaderT LFCfg (StateT LFState IO) a)
        deriving (Functor, Applicative, Monad
                 ,MonadReader LFCfg
                 ,MonadState LFState
                 ,MonadIO)

runLF :: LFCfg -> LFState -> LF a -> IO a
runLF cfg state (LF act) = flip evalStateT state . flip runReaderT cfg $ act

data GuiEvent = ChannelActivated Channel
              | ItemActivated FeedItem
              | ShowChannels
              | QuitLambdaFeed
              | BackToChannels
              | BackToItems
              deriving Show
