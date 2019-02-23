{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import           Brick.Types (Widget)
import qualified Brick.Types as T
import           Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core (vLimitPercent, hLimit, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import           Control.Lens (view)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Lens.Type
import           Control.Monad (void)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import           Debug.Trace (traceShow)
import qualified Graphics.Vty as V

data Names = TopList | BottomList deriving (Ord, Eq, Show)

data UIState = UIState { _uiTopList :: L.List Names Char
                       , _uiBottomList :: L.List Names Char
                       , _uiFocusRing :: F.FocusRing Names
                       }
makeLenses ''UIState

drawUI :: UIState -> [Widget Names]
drawUI s@(UIState _ _ fr) = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case s ^. focusedListL fr. L.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ s ^. focusedListL fr . L.listElementsL
        box list = B.borderWithLabel label $
                   L.renderList listDrawElement True list
        ui = C.vCenter $ vBox [ C.hCenter (box (s ^. focusedListL fr))
                              ]

focusedListL :: F.FocusRing Names -> Lens' UIState (L.List Names Char)
focusedListL f = case F.focusGetCurrent f of
  Just TopList -> uiTopList
  Just BottomList -> uiBottomList
  Nothing -> undefined


appEvent :: UIState -> T.BrickEvent Names e -> T.EventM Names (T.Next UIState)
appEvent s@(UIState l b f) (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'q') [] -> M.halt s
        V.EvKey (V.KChar '\t') [] -> M.continue (UIState l b (F.focusNext f))

        ev -> L.handleListEventVi L.handleListEvent ev (s ^. focusedListL f) >>= \result -> M.continue (s & focusedListL f .~ result)

appEvent l _ = M.continue l

listDrawElement :: Show a => Bool -> a -> Widget Names
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: UIState
initialState =
  UIState (L.list TopList (Vec.fromList []) 1)
          (L.list BottomList (Vec.fromList []) 1)
          (F.focusRing [TopList, BottomList])

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.yellow)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App UIState e Names
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = F.focusRingCursor (view uiFocusRing)
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
