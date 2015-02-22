{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Exception (bracket)
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Local (createCheckpointAndClose)
import           Data.Functor ((<$>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Graphics.Vty (Attr(Attr), MaybeDefault(KeepCurrent,SetTo), black, Key(KChar), defAttr)
import           Graphics.Vty.Widgets.All hiding (wrap)
import           Pipes.Concurrent (send, spawn', bounded, atomically)

import           LambdaFeed
import           LambdaFeed.Types
import           LambdaFeed.Widgets

hn :: Text
hn = "https://news.ycombinator.com/rss"

runningmusic :: Text
runningmusic = "http://www.reddit.com/r/runningmusic/.rss"

nullprogram :: Text
nullprogram = "http://nullprogram.com/feed/"

feedsToFetch :: [Text]
feedsToFetch = [hn
               ,runningmusic
               ,nullprogram
               ,"http://lifehacker.com/index.xml"
               ,"http://www.reddit.com/r/haskell/.rss"
               ]

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
         -> IO (LFCfg, LFState, Collection)
setupGui trigger acid = do
  header <- plainText "λ Feed"
  statusBar <- plainText ""

  loggingList <- newList' 1
  setSelectedUnfocusedAttr loggingList (Just defAttr)
  loggingDetailView <- newArticleWidget
  loggingList `onKeyPressed` viKeys
  loggingCompose <- pure loggingList <--> hBorder <--> vFixed 15 loggingDetailView
  loggingUI <- centered loggingCompose >>= wrap header statusBar

  loggingList `onSelectionChange` \e -> case e of
    SelectionOn _ text _ -> setArticle loggingDetailView [(text,defAttr)]
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

  c <- newCollection
  channelView <- addToCollection c channelUI fgChannel
  itemView <- addToCollection c itemUI fgItems
  contentView <- addToCollection c contentUI fgContent
  loggingView <- addToCollection c loggingUI fgLogging

  fgItems `onKeyPressed` \_ k _ -> case k of
    (KChar 'h') -> trigger BackToChannels
    (KChar 'q') -> trigger BackToChannels
    (KChar 'l') -> activateCurrentItem itemList >> return True
    (KChar 'u') -> trigger ToggleItemVisibility
    (KChar 'L') -> trigger SwitchToLogging
    _ -> return False

  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 'Q') -> trigger QuitLambdaFeed
    (KChar 'A') -> trigger MarkChannelRead
    (KChar 'u') -> trigger ToggleChannelVisibility
    (KChar 'r') -> trigger FetchAll
    (KChar 'l') -> activateCurrentItem channelList >> return True
    (KChar 'L') -> trigger SwitchToLogging
    _ -> return False

  fgContent `onKeyPressed` \_ k _ -> case k of
    (KChar 'h') -> trigger BackToItems
    (KChar 'q') -> trigger BackToItems
    (KChar 'L') -> trigger SwitchToLogging
    _ -> return False

  fgLogging `onKeyPressed` \_ k _ -> case k of
    (KChar 'h') -> trigger BackToChannels
    (KChar 'q') -> trigger BackToChannels
    _ -> return False

  channelList `onItemActivated` \(ActivateItemEvent _ chan _) -> do
    void $ trigger (ChannelActivated chan)

  itemList `onKeyPressed` \_ k _ -> case k of
    (KChar 'i') -> do
      maybeSel <- getSelected itemList
      maybe (return False) (\(_,(item,_)) -> trigger (ExternalCommandOnItem item)) maybeSel
    _ -> return False

  itemList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    void $ trigger (ItemActivated item)

  urls <- T.lines <$> T.readFile "urls"
  let cfg = LFCfg acid switches widgets urls ("bullet-push", ["note"])
      switches = SwitchTo channelView itemView contentView loggingView
      widgets = LFWidgets channelList itemList contentWidget' loggingList statusBar
  return (cfg,initialLFState,c)

viKeys :: Widget (List a b) -> Key -> t -> IO Bool
viKeys = handler
  where handler w (KChar 'j') _ = scrollDown w >> return True
        handler w (KChar 'k') _ = scrollUp w >> return True
        handler w (KChar 'g') _ = scrollToBeginning w >> return True
        handler w (KChar 'G') _ = scrollToEnd w >> return True
        handler _ _ _ = return False

main :: IO ()
main =
  bracket (openLocalState initialDb) createCheckpointAndClose $ \acid -> do
  (output,input,seal) <- spawn' (bounded 1)
  (cfg,s,c) <- setupGui (\e -> atomically $ send output e) acid
  start seal input cfg s
  void . atomically $ send output BackToChannels
  runUi c defaultContext
