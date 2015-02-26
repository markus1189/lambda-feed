{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative (pure)
#endif
import           Control.Exception (bracket)
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.Functor ((<$>))
import           Data.List (sort)
import qualified Data.Text as T
import           Graphics.Vty (Attr(Attr), MaybeDefault(KeepCurrent,SetTo), black, Key(KChar,KEnter,KEsc), Modifier(..), defAttr, white, rgbColor)
import           Graphics.Vty.Widgets.All hiding (wrap)
import qualified Graphics.Vty.Widgets.Text as WT
import           Pipes.Concurrent (send, spawn', bounded, atomically)

import           LambdaFeed
import           LambdaFeed.Actor
import           LambdaFeed.Retrieval
import           LambdaFeed.Types
import           LambdaFeed.Widgets

wrap :: (Show a, Show b, Show c)
     => Widget a
     -> Widget b
     -> Widget c
     -> IO (Widget (Box (Box (Box a HFill) c) (Box b HFill)))
wrap header statusBar widget = do
  header' <- pure header <++> hFill ' ' 1
  statusbar' <- pure statusBar <++> hFill ' ' 1
  setNormalAttribute header' $ Attr KeepCurrent KeepCurrent (SetTo black)
  setNormalAttribute statusbar' $ Attr KeepCurrent KeepCurrent (SetTo black)
  pure header' <--> pure widget <--> pure statusbar'

newList' :: Show b => Int -> IO (Widget (List a b))
newList' i = do l <- newList i
                setSelectedFocusedAttr l (Just myDefHighlight)
                setNormalAttribute l myDefAttr
                return l

setupGui :: (GuiEvent -> IO Bool)
         -> AcidState Database
         -> Actor FetcherControl FetcherEvent
         -> IO (LFCfg, LFState, Collection)
setupGui trigger acid fetcher = do
  header <- plainText "λ Feed"
  statusBar <- plainText ""

  loggingList <- newList' 1
  setSelectedUnfocusedAttr loggingList (Just defAttr)
  loggingDetailView <- textWidget WT.wrap ""
  loggingList `onKeyPressed` viKeys
  loggingCompose <- pure loggingList <--> hBorder <--> vFixed 15 loggingDetailView
  loggingUI <- centered loggingCompose >>= wrap header statusBar

  loggingList `onSelectionChange` \e -> case e of
    SelectionOn _ text _ -> setText loggingDetailView text
    _ -> return ()

  fgLogging <- newFocusGroup
  void $ addToFocusGroup fgLogging loggingList
  void $ addToFocusGroup fgLogging loggingDetailView

  channelList <- newList' 1
  channelList `onKeyPressed` viKeys
  channelUI <- centered channelList >>= wrap header statusBar

  fgChannel <- newFocusGroup
  void $ addToFocusGroup fgChannel channelList

  itemList <- newList' 1
  itemList `onKeyPressed` viKeys
  itemUI <- centered itemList >>= wrap header statusBar

  fgItems <- newFocusGroup
  void $ addToFocusGroup fgItems itemList

  contentWidget' <- newArticleWidget
  contentUI <- wrap header statusBar contentWidget'
  fgContent <- newFocusGroup
  void $ addToFocusGroup fgContent contentWidget'

  editUrlWidget' <- multiLineEditWidget
  setFocusAttribute editUrlWidget' $ white `on` (rgbColor (0::Int) 0 0)
  urlEditUI <- wrap header statusBar editUrlWidget'
  fgUrlEdit <- newFocusGroup
  void $ addToFocusGroup fgUrlEdit editUrlWidget'

  c <- newCollection
  channelView <- addToCollection c channelUI fgChannel
  itemView <- addToCollection c itemUI fgItems
  contentView <- addToCollection c contentUI fgContent
  loggingView <- addToCollection c loggingUI fgLogging
  editUrlView <- addToCollection c urlEditUI fgUrlEdit

  fgItems `onKeyPressed` \_ k ms -> case (k,ms) of
    (KChar 'h',[]) -> trigger BackToChannels
    (KChar 'q',[]) -> trigger BackToChannels
    (KChar 'l',[]) -> trigger ToggleItemVisibility
    (KChar 'A',[]) -> trigger $ Compose MarkCurrentChannelRead BackToChannels
    _ -> return False

  fgChannel `onKeyPressed` \_ k ms -> case (k,ms) of
    (KChar 'Q',[]) -> trigger QuitLambdaFeed
    (KChar 'l',[]) -> trigger ToggleChannelVisibility
    (KChar 'u',[]) -> trigger FetchAll
    (KChar 'E',[]) -> trigger EditUrls
    (KChar 'L',[]) -> trigger SwitchToLogging
    _ -> return False

  fgContent `onKeyPressed` \_ k ms -> case (k,ms) of
    (KChar 'h',[]) -> trigger BackToItems
    (KChar 'q',[]) -> trigger BackToItems
    _ -> return False

  fgLogging `onKeyPressed` \_ k ms -> case (k,ms) of
    (KChar 'h',[]) -> trigger BackToChannels
    (KChar 'q',[]) -> trigger BackToChannels
    _ -> return False

  channelList `onItemActivated` \(ActivateItemEvent _ chan _) -> do
    void $ trigger (ChannelActivated chan)

  channelList `onKeyPressed` \this k ms -> case (k,ms) of
    (KChar 'A',[]) -> do
      maybeSel <- getSelected this
      case maybeSel of
        Nothing -> return True
        Just (_,(chan,_)) -> trigger (MarkChannelRead chan)
    _ -> return False

  itemList `onKeyPressed` \_ k ms -> case (k,ms) of
    (KChar 'i',[]) -> do
      maybeSel <- getSelected itemList
      maybe (return False) (\(_,(item,_)) -> trigger (ExternalCommandOnItem item)) maybeSel
    _ -> return False

  itemList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    void $ trigger (ItemActivated item)

  loggingList `onItemAdded` \_ -> do
    size <- getListSize loggingList
    when (size >= 50) $ void $ removeFromList loggingList 0

  editUrlWidget' `onKeyPressed` \_ k modifier -> do
    let run f = applyEdit f editUrlWidget' >> return True
    case (k,modifier) of
      (KChar '<', sort -> [MMeta]) -> run $ moveCursor (0,0)
      (KChar '>', sort -> [MMeta]) -> do
        numLines <- length . T.lines <$> getEditText editUrlWidget'
        run $ moveCursor (numLines - 1,0)
      (KChar 'p', [MCtrl]) -> run moveUp
      (KChar 'n', [MCtrl]) -> run moveDown
      (KEnter, [MMeta]) -> trigger AcceptUrlEditing
      (KEsc, []) -> trigger AbortUrlEditing
      _ -> return False

  let cfg = LFCfg acid switches widgets ("bullet-push", ["link"]) (void . trigger) fetcher "lambda-feed-urls"
      switches = SwitchTo channelView itemView contentView loggingView editUrlView
      widgets = LFWidgets channelList itemList contentWidget' loggingList statusBar header editUrlWidget'
      s = initialLFState
  return (cfg,s,c)

viKeys :: Widget (List a b) -> Key -> [Modifier] -> IO Bool
viKeys = handler
  where handler w (KChar 'j') [] = scrollDown w >> return True
        handler w (KChar 'k') [] = scrollUp w >> return True
        handler w (KChar 'g') [] = scrollToBeginning w >> return True
        handler w (KChar 'G') [] = scrollToEnd w >> return True
        handler _ _ _ = return False

main :: IO ()
main = bracket (openLocalState initialDb) createCheckpointAndClose $ \acid -> do
  fetcher <- fetchActor (30 * 1000 * 1000)
  (output,input,seal) <- spawn' (bounded 10)
  (cfg,s,c) <- setupGui (\e -> atomically $ send output e) acid fetcher
  lambdaFeed seal (fetcher ^. actorOutbox) input cfg s
  void . atomically $ send output BackToChannels
  void . atomically $ send output FetchAll
  runUi c defaultContext
