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
import           Data.Text (Text)
import           Graphics.Vty (Attr(Attr), MaybeDefault(KeepCurrent,SetTo), black, Key(KChar))
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

  fgItems `onKeyPressed` \_ k _ -> case k of
    (KChar 'h') -> trigger BackToChannels
    (KChar 'u') -> trigger ToggleItemVisibility
    _ -> return False

  fgChannel `onKeyPressed` \_ k _ -> case k of
    (KChar 'Q') -> trigger QuitLambdaFeed
    (KChar 'A') -> trigger MarkChannelRead
    (KChar 'u') -> trigger ToggleChannelVisibility
    (KChar 'r') -> trigger FetchAll
    _ -> return False

  fgContent `onKeyPressed` \_ k _ -> case k of
    (KChar 'h') -> trigger BackToItems
    _ -> return False

  channelList `onItemActivated` \(ActivateItemEvent _ chan _) -> do
    void $ trigger (ChannelActivated chan)

  itemList `onItemActivated` \(ActivateItemEvent _ item _) -> do
    void $ trigger (ItemActivated item)

  let cfg = LFCfg acid switches widgets feedsToFetch
      switches = SwitchTo channelView itemView contentView
      widgets = LFWidgets channelList itemList contentWidget' statusBar
  return (cfg,initialLFState,c)

viKeys :: Widget (List a b) -> Key -> t -> IO Bool
viKeys = handler
  where handler w (KChar 'j') _ = scrollDown w >> return True
        handler w (KChar 'k') _ = scrollUp w >> return True
        handler w (KChar 'g') _ = scrollToBeginning w >> return True
        handler w (KChar 'G') _ = scrollToEnd w >> return True
        handler w (KChar ':') _ = setSelected w 5 >> return True
        handler _ _ _ = return False

main :: IO ()
main =
  bracket (openLocalState initialDb) createCheckpointAndClose $ \acid -> do
  (output,input,seal) <- spawn' (bounded 1)
  (cfg,s,c) <- setupGui (\e -> atomically $ send output e) acid
  start seal input cfg s
  atomically $ send output BackToChannels
  runUi c defaultContext
