{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module LambdaFeed.Types (Channel(Channel)
                        ,channelTitle
                        ,channelUrl
                        ,channelFetchUrl

                        ,FeedItem(FeedItem)
                        ,itemTitle
                        ,itemUrl
                        ,itemCommentUrl
                        ,itemContent
                        ,itemChannel
                        ,itemPubDate
                        ,itemId

                        ,ItemId(IdFromFeed, IdFromContentSHA)
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
                        ,seenItems

                        ,Visibility(..)

                        ,AllItems(..)
                        ,GetItems(..)
                        ,GetChannels(..)
                        ,UpdateFeeds(..)
                        ,MarkAsRead(..)
                        ,PurgeOld(..)

                        ,LF
                        ,runLF

                        ,LFCfg(LFCfg)
                        ,lfAcid
                        ,lfSwitch
                        ,lfWidgets
                        ,lfExternalCommand
                        ,lfFetcherActor
                        ,lfUrlsFile
                        ,triggerEvt

                        ,SwitchTo(SwitchTo)
                        ,switchToChannels
                        ,switchToItems
                        ,switchToContent
                        ,switchToLogging
                        ,switchToEditUrl

                        ,LFWidgets(LFWidgets)
                        ,channelWidget
                        ,itemWidget
                        ,contentWidget
                        ,loggingWidget
                        ,statusBarWidget
                        ,headerWidget
                        ,editUrlWidget

                        ,LFState
                        ,initialLFState
                        ,lfVisibility
                        ,lfCurrentChannel
                        ,lfUpdateAsync

                        ,GuiEvent(..)
                        ,RetrievalError(..)
                        ,FetcherControl(..)
                        ,FetcherEvent(..)

                        ,collectNewItems
                        ,guidOrSHA
                        ,feedsWithGuid
                        ,nonAcidMarkAsRead
                        ,nonAcidUpdateFeeds
                        ) where

import           Control.Applicative
import           Control.Concurrent.Async (Async)
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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.SafeCopy
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Time
import           Graphics.Vty.Widgets.All (Widget, List, FormattedText, Edit)
import           Network.HTTP.Client (HttpException)

import           LambdaFeed.Actor

data RetrievalError = RetrievalHttpError Text HttpException
                    | TimeOutDuringRetrieve Text Int
                    | FeedParseError Text
                    deriving (Show)
data Channel = Channel { _channelTitle :: Text
                       , _channelUrl :: Maybe Text
                       , _channelFetchUrl :: Text
                       } deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Channel
$(deriveSafeCopy 1 'base ''Channel)

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

data Database = Database { _unreadFeeds :: !(Map Channel (Seq FeedItem))
                         , _readFeeds :: !(Map Channel (Seq FeedItem))
                         , _seenItems :: !(Map Channel (Set ItemId))
                         } deriving (Show,Eq,Data,Typeable)
$(deriveSafeCopy 3 'base ''Database)
makeLenses ''Database

data Visibility = OnlyUnread | OnlyRead | ReadAndUnread
  deriving (Show, Eq, Data, Typeable)
$(deriveSafeCopy 0 'base ''Visibility)

data LFState = LFState { _lfVisibility :: Visibility
                       , _lfCurrentChannel :: Maybe Channel
                       , _lfUpdateAsync :: Maybe (Async ())
                       }
makeLenses ''LFState

initialLFState :: LFState
initialLFState = LFState OnlyUnread Nothing Nothing

data SwitchTo = SwitchTo { _switchToChannels :: IO ()
                         , _switchToItems :: IO ()
                         , _switchToContent :: IO ()
                         , _switchToLogging :: IO ()
                         , _switchToEditUrl :: IO ()
                         }
makeLenses ''SwitchTo

data LFWidgets = LFWidgets { _channelWidget :: Widget (List Channel FormattedText)
                           , _itemWidget :: Widget (List FeedItem FormattedText)
                           , _contentWidget :: Widget (List Text FormattedText)
                           , _loggingWidget :: Widget (List Text FormattedText)
                           , _statusBarWidget :: Widget FormattedText
                           , _headerWidget :: Widget FormattedText
                           , _editUrlWidget :: Widget Edit
                           }
makeLenses ''LFWidgets

data GuiEvent = ChannelActivated Channel
              | ItemActivated FeedItem
              | ShowChannels
              | QuitLambdaFeed
              | BackToChannels
              | BackToItems
              | MarkCurrentChannelRead
              | MarkChannelRead Channel
              | MarkAllChannelsRead
              | ToggleChannelVisibility
              | ToggleItemVisibility
              | FetchAll
              | ExternalCommandOnItem Bool FeedItem
              | SwitchToLogging
              | EditUrls
              | AcceptUrlEditing
              | AbortUrlEditing
              | Compose GuiEvent GuiEvent
              | PurgeOldItems
              deriving Show

data FetcherControl = StartFetch [Text] deriving Show

data FetcherEvent = StartedSingleFetch UTCTime Text (Int,Int)
                  | CompletedSingleFetch UTCTime Text (Seq FeedItem)
                  | ErrorDuringFetch Text RetrievalError
                  | FetchFinished UTCTime
                  deriving Show


data LFCfg = LFCfg { _lfAcid :: AcidState Database
                   , _lfSwitch :: SwitchTo
                   , _lfWidgets :: LFWidgets
                   , _lfExternalCommand :: (String, [String])
                   , _triggerEvt :: GuiEvent -> IO ()
                   , _lfFetcherActor :: Actor FetcherControl FetcherEvent
                   , _lfUrlsFile :: FilePath
                   }
makeLenses ''LFCfg

newtype LF a = LF (ReaderT LFCfg (StateT LFState IO) a)
        deriving (Functor, Applicative, Monad
                 ,MonadReader LFCfg
                 ,MonadState LFState
                 ,MonadIO)

runLF :: LFCfg -> LFState -> LF a -> IO a
runLF cfg state (LF act) = flip evalStateT state . flip runReaderT cfg $ act

purgeOld :: Update Database ()
purgeOld = readFeeds .= Map.empty

getChannels :: Visibility -> Query Database [Channel]
getChannels OnlyUnread = Map.keys <$> view unreadFeeds
getChannels OnlyRead = Map.keys <$> view readFeeds
getChannels ReadAndUnread = do
  u <- Map.keysSet <$> view unreadFeeds
  r <- Map.keysSet <$> view readFeeds
  return $ toList (u `Set.union` r)

getItems :: Visibility -> Channel -> Query Database (Seq FeedItem)
getItems OnlyUnread c = view (unreadFeeds . at c . non Seq.empty)
getItems OnlyRead c = view (readFeeds . at c . non Seq.empty)
getItems ReadAndUnread c = do
  unreadItems <- view (unreadFeeds . at c . non Seq.empty)
  readItems <- view (readFeeds . at c . non Seq.empty)
  return . reverseDateSort $ readItems >< unreadItems

allItems :: Query Database (Map Channel (Seq FeedItem), Map Channel (Seq FeedItem))
allItems = (,) <$> view unreadFeeds <*> view readFeeds

markAsRead :: Channel -> Update Database ()
markAsRead = nonAcidMarkAsRead

nonAcidMarkAsRead :: MonadState Database m => Channel -> m ()
nonAcidMarkAsRead c = do
  maybeToMove <- unreadFeeds . at c <<.= Nothing
  case maybeToMove of
    Nothing -> return ()
    Just toMove ->
      readFeeds . at c . non Seq.empty %= reverseDateSort . (>< toMove)

reverseDateSort :: Seq FeedItem -> Seq FeedItem
reverseDateSort = Seq.sortBy (flip compare `on` view itemPubDate)

updateFeeds :: Seq FeedItem -> Update Database ()
updateFeeds = nonAcidUpdateFeeds

nonAcidUpdateFeeds :: MonadState Database m => Seq FeedItem -> m ()
nonAcidUpdateFeeds feeds = do
  seen <- use seenItems
  unreadFeeds %= fmap reverseDateSort . Map.unionWith (><) (collectNewItems seen feeds)
  seenItems %= Map.union (feedsWithGuid feeds)

feedsWithGuid :: Foldable f => f FeedItem -> Map Channel (Set ItemId)
feedsWithGuid feeds = foldl' step Map.empty feeds
  where step :: Map Channel (Set ItemId) -> FeedItem -> Map Channel (Set ItemId)
        step acc item =
          Map.insertWith Set.union
                         (item ^. itemChannel)
                         (fromMaybe Set.empty (Set.singleton <$> (guidOrSHA item)))
                         acc

collectNewItems :: (Functor f, Foldable f)
                 => Map Channel (Set ItemId)
                 -> f FeedItem
                 -> Map Channel (Seq FeedItem)
collectNewItems seen = foldl' step Map.empty
  where isOld item =
          flip (maybe True) (guidOrSHA item) $ \guid ->
            Set.member guid (Map.findWithDefault Set.empty (item ^. itemChannel) seen)
        isNew = not . isOld

        step :: Map Channel (Seq FeedItem) -> FeedItem -> Map Channel (Seq FeedItem)
        step acc item = if isNew item then inserted else acc
          where inserted = acc & at (item ^. itemChannel) . non Seq.empty %~ (|> item)


guidOrSHA :: FeedItem -> Maybe ItemId
guidOrSHA i = view itemId i <|> (review _IdFromContentSHA . showDigest) <$> sha
  where sha = sha1 . view lazy . T.encodeUtf8 <$> (maybeItemContent <> maybeChannelTitle)
        maybeItemContent = view itemContent i
        maybeChannelTitle = view (itemChannel . channelUrl) i

$(makeAcidic ''Database ['getItems
                        ,'allItems
                        ,'updateFeeds
                        ,'markAsRead
                        ,'getChannels
                        ,'purgeOld
                        ])

initialDb :: Database
initialDb = Database Map.empty Map.empty Map.empty
