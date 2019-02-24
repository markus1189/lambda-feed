{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import qualified Brick.AttrMap as Brick
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import           Brick.Types
import           Brick.Util (on)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Async (async)
import           Control.Lens.Operators
import           Control.Monad (void)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import qualified Text.Feed.Query as FeedQuery

import           Events
import           Feed
import           View

appEvent :: UIState -> BrickEvent WidgetName CustomEvent -> EventM WidgetName (Next UIState)
appEvent s (AppEvent (DownloadedFeed f)) = Brick.continue $ s & uiFeedList %~ L.listInsert 0 f
appEvent s@(UIState _ _ f _) (VtyEvent e) =
    case e of
      Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt s
      MoveLeft -> case f of
        FeedList -> Brick.continue s
        ItemList -> Brick.continue $ s & uiFocus .~ FeedList & uiItemList %~ L.listClear
        ItemView -> Brick.continue $ s & uiFocus .~ ItemList
      MoveRight -> case f of
        FeedList -> case L.listSelectedElement (s ^. uiFeedList) of
          Nothing -> Brick.continue s
          Just (_, feed) -> Brick.continue $ s & uiFocus .~ ItemList & uiItemList %~ L.listReplace (V.fromList (FeedQuery.getFeedItems feed)) (Just 0)
        ItemList -> case L.listSelectedElement (s ^. uiItemList) of
          Nothing -> Brick.continue s
          Just _ -> Brick.continue $ s & uiFocus .~ ItemView
        ItemView -> Brick.continue $ s & uiFocus .~ ItemList
      ev -> case f of
        FeedList -> L.handleListEventVi L.handleListEvent ev (s ^. uiFeedList) >>= \result -> Brick.continue (s & uiFeedList .~ result)
        ItemList -> L.handleListEventVi L.handleListEvent ev (s ^. uiItemList) >>= \result -> Brick.continue (s & uiItemList .~ result)
        ItemView -> Brick.continue s

appEvent l _ = Brick.continue l

initialState :: UIState
initialState = UIState (L.list FeedList (V.fromList []) 1)
                       (L.list ItemList (V.fromList []) 1)
                       FeedList
                       []

theMap :: Brick.AttrMap
theMap = Brick.attrMap Vty.defAttr
    [ (L.listAttr, Vty.white `on` Vty.black)
    , (L.listSelectedAttr, Vty.black `on` Vty.yellow)
    , (itemViewAttr, Vty.white `on` Vty.black)
    ]

theApp :: Brick.App UIState CustomEvent WidgetName
theApp =
    Brick.App { Brick.appDraw = drawUI
          , Brick.appChooseCursor = \s cs -> Brick.showCursorNamed (s ^. uiFocus) cs
          , Brick.appHandleEvent = appEvent
          , Brick.appStartEvent = return
          , Brick.appAttrMap = const theMap
          }

main :: IO ()
main = do
  eventChan <- Brick.newBChan 10
  _ <- async (downloadFeeds eventChan)
  void $ Brick.customMain (Vty.mkVty Vty.defaultConfig) (Just eventChan) theApp initialState
