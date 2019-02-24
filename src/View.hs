{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module View where

import qualified Brick.AttrMap as Brick
import           Brick.Types
import qualified Brick.Widgets.Center as Brick
import           Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.List as L
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import           Data.Time.LocalTime (ZonedTime)
import qualified Graphics.Vty as Vty
import qualified Text.Feed.Query as FeedQuery
import           Text.Feed.Types (Feed, Item)

import           Feed

data WidgetName = FeedList | ItemList | ItemView deriving (Ord, Eq, Show)

data UIState = UIState { _uiFeedList :: L.List WidgetName Feed
                       , _uiItemList :: L.List WidgetName Item
                       , _uiFocus :: WidgetName
                       , _uiUrlList :: [String]
                       }
makeLenses ''UIState

pattern MoveRight :: Vty.Event
pattern MoveRight = Vty.EvKey (Vty.KChar 'l') []

pattern MoveLeft :: Vty.Event
pattern MoveLeft = Vty.EvKey (Vty.KChar 'h') []

drawUI :: UIState -> [Widget WidgetName]
drawUI s@(UIState _ _ FeedList _) = [Brick.vCenter $ Brick.hCenter (L.renderList listDrawFeedElement True (s ^. uiFeedList))]
drawUI s@(UIState _ _ ItemList _) = [Brick.vCenter $ Brick.hCenter (L.renderList listDrawItemElement True (s ^. uiItemList))]
drawUI s@(UIState _ _ ItemView _) = maybeToList (drawItemView . snd <$> L.listSelectedElement (s ^. uiItemList))

drawItemView :: Item -> Widget n
drawItemView item = Brick.withAttr itemViewAttr $ Brick.vBox [ Brick.txt "Title: " <+> (Brick.txt $ fromMaybe "<no title>" (FeedQuery.getItemTitle item))
                                                             , Brick.txt "Link: " <+> (maybe (Brick.txt "") (\l -> Brick.hyperlink l (Brick.txt l)) (FeedQuery.getItemLink item))
                                                             , Brick.txt "Published: " <+> (Brick.txt $ maybe "<no date>" (T.pack . show) (FeedQuery.getItemPublishDate @ZonedTime item))
                                                             , Brick.txt "Id: " <+> (Brick.txt $ maybe "<no id>" (T.pack . show) (FeedQuery.getItemId item))
                                                             , Brick.txt "Summary: " <+> (Brick.txt $ maybe "<no summary>" (T.pack . show) (FeedQuery.getItemSummary item))
                                                             , Brick.txt "Description: " <+> (Brick.txt $ maybe "<no description>" (T.pack . show) (FeedQuery.getItemDescription item))
                                                             ]

listDrawFeedElement :: Bool -> Feed -> Widget WidgetName
listDrawFeedElement _ feed = Brick.padRight Max $ countTxt <+> Brick.txt " " <+> Brick.txt (FeedQuery.getFeedTitle feed)
  where numItems = length (FeedQuery.getFeedItems feed)
        countTxt = Brick.txt "[" <+> Brick.hLimit 3 (Brick.padLeft Max (Brick.txt . T.pack . show $ numItems)) <+> Brick.txt "]"

listDrawItemElement :: Bool -> Item -> Widget WidgetName
listDrawItemElement _ item = Brick.padRight Max $ Brick.txt $ getItemTitle' item

customAttr :: Brick.AttrName
customAttr = L.listSelectedAttr <> "custom"

itemViewAttr :: Brick.AttrName
itemViewAttr = "itemviewattr"
