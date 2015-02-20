{-# LANGUAGE OverloadedStrings #-}
module LambdaFeed (start) where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (STM)
import           Control.Lens (view, non, at)
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.Acid.Advanced (query')
import           Data.Foldable
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens (_Text)
import           Data.Time (UTCTime)
import           Data.Time (formatTime, defaultTimeLocale, rfc822DateFormat)
import           Formatting (sformat, left, (%), (%.), stext, int)
import           Formatting.Time (monthNameShort, dayOfMonth)
import           Graphics.Vty (Attr)
import           Graphics.Vty.Widgets.All (clearList, plainText, addToList)
import           Graphics.Vty.Widgets.EventLoop (schedule, shutdownUi)
import           Pipes
import           Pipes.Concurrent (fromInput, atomically, Input)
import           System.Exit (exitSuccess)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (readProcess)

import           LambdaFeed.Types
import           LambdaFeed.Widgets

showChannels :: LF ()
showChannels = view (lfSwitch . switchToChannels) >>= (liftIO . schedule)

showItemsFor :: Channel -> LF ()
showItemsFor chan = do
  widget <- view (lfWidgets . itemWidget)
  acid <- view lfAcid
  items <- query' acid UnreadItems
  switchAction <- view (lfSwitch . switchToItems)
  void . liftIO $ schedule $ saveSelection widget $ do
    clearList widget
    case items ^. at chan of
      Nothing -> return ()
      Just is -> for_ (zip [1..] (toList is)) $ \(i,itm) -> do
        plainText (renderItem i itm) >>= addToList widget itm
    switchAction

renderItem :: Int -> FeedItem -> Text
renderItem i item = sformat fmt i pdate pdate description
  where description =
          fromJust (view itemTitle item <|> view itemUrl item <|> Just "<unknown>")
        pdate = view itemPubDate item
        fmt = (left 4 ' ' %. int)
            % "  "
            % monthNameShort
            % " "
            % dayOfMonth
            % "   "
            % stext

showContentFor :: FeedItem -> LF ()
showContentFor item = do
  widget <- view (lfWidgets . contentWidget)
  switchAction <- view (lfSwitch . switchToContent)
  liftIO $ schedule $ do
    setArticle widget (display (renderItemContent item))
    switchAction

renderItemContent :: FeedItem -> RenderedItem
renderItemContent i =
  RenderedItem feedTitle' itemTitle' itemLink' itemComments' pubDate' pandocResult guid
  where pandocRender = unsafePerformIO . readProcess "/usr/bin/pandoc" ["-f"
                                                                       ,"html"
                                                                       ,"-t"
                                                                       ,"markdown"
                                                                       ,"--reference-links"
                                                                       ]
        content = view (itemContent . non "Cannot render content.") i
        pandocResult = content & _Text %~ pandocRender
        feedTitle' = view (itemChannel . channelTitle) $ i
        itemTitle' = view (itemTitle . non "<No title>") i
        itemLink' = stripSlash . view (itemUrl . non "<No link>") $ i
        itemComments' = stripSlash $ view (itemCommentUrl . non "<No comments link>") i
        guid = maybe "<No identifier>" (T.pack . show) $ view itemId i
        pubDate' = formatPubDate . view itemPubDate $ i
        formatPubDate :: UTCTime -> Text
        formatPubDate = T.pack . formatTime defaultTimeLocale rfc822DateFormat
        stripSlash = T.dropWhileEnd (=='/')

display :: RenderedItem -> [(Text,Attr)]
display r = [("Feed: " <> view renderedFeed r, myHeaderHighlight)
            ,("Title: " <> view renderedItemTitle r, myHeaderHighlight)
            ,("Link: " <> view renderedUrl r, myHeaderHighlight)
            ,("ID: " <> view renderedId r, myHeaderHighlight)
            ,("Comments: " <> view renderedCommentUrl r, myHeaderHighlight)
            ,("Date: " <> view renderedPubDate r, myHeaderHighlight)
            ,("\n", myDefAttr)
            ,(view renderedContent r, myDefAttr)
            ]

start :: STM () -> Input GuiEvent -> LFCfg -> LFState -> IO ()
start seal input cfg s =
  void . forkIO $ runLF cfg s . runEffect $ fromInput input >-> guiEventHandler seal

guiEventHandler :: STM () -> Consumer GuiEvent LF ()
guiEventHandler seal = forever $ await >>= lift . handle
  where handle :: GuiEvent -> LF ()
        handle ShowChannels = showChannels
        handle (ChannelActivated chan) = showItemsFor chan
        handle (ItemActivated item) = showContentFor item
        handle QuitLambdaFeed = liftIO $ atomically seal >> shutdownUi >> exitSuccess
        handle BackToChannels = view (lfSwitch . switchToChannels) >>= liftIO . schedule
        handle BackToItems = view (lfSwitch . switchToItems) >>= liftIO . schedule
