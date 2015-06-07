{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaFeed (lambdaFeed) where

#if MIN_VERSION_time(1,5,0)
import           Data.Time (defaultTimeLocale, rfc822DateFormat)
#else
import           System.Locale (defaultTimeLocale, rfc822DateFormat)
#endif
#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable (traverse)
#endif

import           Control.Applicative
import           Control.Concurrent (forkIO,threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM (STM)
import           Control.Exception.Extra (retry, try_)
import           Control.Lens (view, non, use, Lens')
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.Acid
import           Data.Acid.Advanced (query', update', MethodState, MethodResult)
import           Data.Foldable
import           Data.List (sortBy, findIndex)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lens (_Text)
import           Data.Time (UTCTime, utcToLocalTime, formatTime, getCurrentTimeZone, getCurrentTime)
import           Formatting (sformat, left, (%), (%.), stext, int)
import           Formatting.Time (monthNameShort, dayOfMonth, hms)
import           Graphics.Vty (Attr)
import           Graphics.Vty.Widgets.All (Widget, List, FormattedText, clearList, plainText, addToList, setText, appendText, setEditText, getEditText)
import           Graphics.Vty.Widgets.EventLoop (schedule, shutdownUi)
import           LambdaFeed.Actor
import           LambdaFeed.Types
import           LambdaFeed.Widgets
import           Pipes
import           Pipes.Concurrent (fromInput, atomically, Input, send)
import           System.Directory (doesFileExist)
import           System.Exit (exitSuccess, ExitCode(..))
import           System.Process (runInteractiveProcess, waitForProcess)
import           Text.Pandoc (def)
import qualified Text.Pandoc as Pandoc

queryAcid :: (QueryEvent e, MethodState e ~ Database) => e -> LF (MethodResult e)
queryAcid x = do
  acid <- view lfAcid
  liftIO $ query' acid x

updateAcid :: (UpdateEvent e, MethodState e ~ Database) => e -> LF (EventResult e)
updateAcid x = do
  acid <- view lfAcid
  liftIO $ update' acid x

showChannels :: LF ()
showChannels = switchUsing switchToChannels

switchUsing :: Lens' SwitchTo (IO ()) -> LF ()
switchUsing l = view (lfSwitch . l) >>= (liftIO . schedule)

showItemsFor :: Channel -> LF ()
showItemsFor chan = do
  lfCurrentChannel ?= chan
  widget <- view (lfWidgets . itemWidget)
  vis <- use lfVisibility
  items <- queryAcid (GetItems vis chan)
  switchAction <- view (lfSwitch . switchToItems)
  void . liftIO $ schedule $ do
    clearList widget
    for_ (zip [1..] (toList items)) $ \(i,itm) ->
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
  where content = view (itemContent . non "Cannot render content") i
        pandocResult = content & _Text %~ pandocRender
        feedTitle' = sanitizedTitle $ view itemChannel i
        itemTitle' = view (itemTitle . non "<No title>") i
        itemLink' = stripSlash . view (itemUrl . non "<No link>") $ i
        itemComments' = stripSlash $ view (itemCommentUrl . non "<No comments link>") i
        guid = maybe "<No identifier>" (T.pack . show) $ view itemId i
        pubDate' = formatPubDate . view itemPubDate $ i
        formatPubDate :: UTCTime -> Text
        formatPubDate = T.pack . formatTime defaultTimeLocale rfc822DateFormat
        stripSlash = T.dropWhileEnd (=='/')

pandocRender :: String -> String
pandocRender = either (const "<Could not render this content>")
                      (Pandoc.writeMarkdown def)
             . Pandoc.readHtml def

sanitizedTitle :: Channel -> Text
sanitizedTitle chan = head $ T.lines $ T.strip $ if T.null ctitle
                                                   then view channelFetchUrl chan
                                                   else ctitle
  where ctitle = view channelTitle chan

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

lambdaFeed :: STM () -> Input FetcherEvent -> Input GuiEvent -> LFCfg -> LFState -> IO ()
lambdaFeed seal fetcherEvents guiEvents cfg s = do
  exists <- doesFileExist (cfg ^. lfUrlsFile)
  unless exists $ TIO.writeFile (cfg ^. lfUrlsFile) ""
  let inputs = (Left <$> fetcherEvents) <> (Right <$> guiEvents)
  void . forkIO $ runLF cfg s . runEffect $ fromInput inputs >-> forever (do
    event <- await
    case event of
      Left fetcherEvent -> lift (handleFetcherEvent fetcherEvent)
      Right guiEvent -> lift (handleGUIEvent seal guiEvent))

readUrlsFromFile :: LF [Text]
readUrlsFromFile = do
  path <- view lfUrlsFile
  liftIO $ T.lines <$> TIO.readFile path

fetchAllFeeds :: LF ()
fetchAllFeeds = do
  fetcher <- view lfFetcherActor
  us <- readUrlsFromFile
  void . liftIO . atomically . send (fetcher ^. actorInbox) $ StartFetch us

resetHeader :: LF ()
resetHeader = do
  w <- view (lfWidgets.headerWidget)
  liftIO $ setText w "λ Feed"

handleFetcherEvent :: FetcherEvent -> LF ()
handleFetcherEvent (StartedSingleFetch _ url (cur,total)) = do
  let msg = sformat ("(" % int % "/" % int % ") Fetching " % stext) cur total url
  statusSet msg
  logIt' msg
handleFetcherEvent (CompletedSingleFetch _ url items) = do
  unless (Seq.null items) $ do
    updateAcid (UpdateFeeds items)
    updateChannelWidget
  logIt' ("Fetched " <> T.pack (show (Seq.length items)) <> " items from: " <> url )
handleFetcherEvent (FetchFinished _) = do
  (liftIO . createCheckpoint) =<< view lfAcid
  statusSet "Fetch complete"
  logIt' "Fetching finished"
  trigger <- view triggerEvt
  liftIO . void . async $ threadDelay (30 * 60 * 1000 * 1000) >> trigger FetchAll
handleFetcherEvent (ErrorDuringFetch url err) =
  logIt ("Failed to fetch " <> url) (T.pack $ show err)

handleGUIEvent :: STM () -> GuiEvent -> LF ()
handleGUIEvent seal = handle
  where handle :: GuiEvent -> LF ()
        handle (Compose e1 e2) = handle e1 >> handle e2
        handle FetchAll = fetchAllFeeds
        handle ShowChannels = showChannels
        handle (ChannelActivated chan) = do
          w <- view (lfWidgets . headerWidget)
          liftIO $ appendText w (" > " <> sanitizedTitle chan)
          showItemsFor chan
        handle (ItemActivated item) = showContentFor item
        handle PurgeOldItems = do
          updateAcid PurgeOld
          statusSet "Purged old items"
          updateChannelWidget
        handle QuitLambdaFeed = liftIO $ atomically seal >> shutdownUi >> exitSuccess
        handle BackToChannels = do
          resetHeader
          updateChannelWidget
          showChannels
        handle BackToItems = switchUsing switchToItems
        handle MarkCurrentChannelRead = markCurrentChannelRead
        handle (MarkChannelRead chan) = markChannelRead chan
        handle MarkAllChannelsRead = markAllChannelsRead
        handle ToggleChannelVisibility = do
          vis <- use lfVisibility
          lfVisibility .= case vis of
                            OnlyUnread -> ReadAndUnread
                            ReadAndUnread -> OnlyUnread
                            i -> i
          updateChannelWidget
        handle ToggleItemVisibility = do
           currentChannel <- use lfCurrentChannel
           vis <- use lfVisibility
           lfVisibility .= case vis of
                             OnlyUnread -> ReadAndUnread
                             ReadAndUnread -> OnlyUnread
                             i -> i
           for_ currentChannel $ \curr -> showItemsFor curr
        handle (ExternalCommandOnItem item) = executeExternal item
        handle SwitchToLogging = switchUsing switchToLogging
        handle EditUrls = do
          prepareEditUrls
          switchUsing switchToEditUrl
        handle AbortUrlEditing = do
          statusSet "Edits discarded"
          switchUsing switchToChannels
        handle AcceptUrlEditing = do
          w <- view (lfWidgets.editUrlWidget)
          newUrls <- liftIO $ getEditText w
          file <- view lfUrlsFile
          liftIO $ TIO.writeFile file newUrls
          statusSet "Updated urls"
          switchUsing switchToChannels

prepareEditUrls :: LF ()
prepareEditUrls = do
  w <- view (lfWidgets.editUrlWidget)
  us <- readUrlsFromFile
  liftIO $ setEditText w (T.intercalate "\n" us)

markChannelRead :: Channel -> LF ()
markChannelRead chan = do
  updateAcid (MarkAsRead chan)
  updateChannelWidget

markAllChannelsRead :: LF ()
markAllChannelsRead = do
  widget <- view (lfWidgets . channelWidget)
  void (liftIO (getListItems widget) >>= traverse (updateAcid . MarkAsRead))
  updateChannelWidget

markCurrentChannelRead :: LF ()
markCurrentChannelRead = do
  currentChannel <- use lfCurrentChannel
  for_ currentChannel markChannelRead

executeExternal :: FeedItem -> LF ()
executeExternal item = do
  logCmd <- getLogCommand
  statusLogCmd <- getStatusLogCommand
  (command,args) <- view lfExternalCommand
  let maybeUrlTitle =
        (,) <$> (view itemCommentUrl item <|> view itemUrl item) <*> view itemTitle item
  for_ maybeUrlTitle $ \(url,title) -> do
    logIt' $ T.pack command <> " "
          <> T.intercalate " " (fmap T.pack args)
          <> " <" <> title <> ">"
          <> " <" <> url <> ">"
    liftIO . forkIO $ do
      res <- try_ . retry 3 $ do
               (_,_,_,p) <- runInteractiveProcess
                              command
                              (args ++ [T.unpack title, T.unpack url])
                              Nothing
                              Nothing
               waitForProcess p
      case res of
        Left e -> do
          statusLogCmd "External command failed (see log)"
          logCmd "Command failed" (T.pack . show $ e)
        Right ExitSuccess -> return ()
        Right (ExitFailure exitCode) -> do
          statusLogCmd "External command failed (see log)"
          logCmd "Command failed with exit code: " (T.pack . show $ exitCode)

updateChannelWidget :: LF ()
updateChannelWidget = do
  widget <- view (lfWidgets . channelWidget)
  vis <- use lfVisibility
  urls <- readUrlsFromFile
  (unreadItems,readItems) <- queryAcid AllItems
  visibleChannels <- sortAsGiven urls <$> queryAcid (GetChannels vis)
  liftIO . schedule . saveSelection widget $ do
    clearList widget
    for_ visibleChannels $ \chan -> do
      let numItemsUnread = maybe 0 Seq.length $ Map.lookup chan unreadItems
          numItemsRead = maybe 0 Seq.length $ Map.lookup chan readItems
          total = numItemsRead + numItemsUnread
          fmtTotal = sformat ("(" % int % "/" % int % ")") numItemsUnread total
      lbl <- liftIO $ plainText $ sformat ((left 11 ' ' %. stext) % " " % stext)
                                          fmtTotal (sanitizedTitle chan)
      addToList widget chan lbl

sortAsGiven :: Foldable f => [Text] -> f Channel -> [Channel]
sortAsGiven urls cs = sortBy (comparing indexAsGiven) (toList cs)
  where indexAsGiven chan = findIndex (fetchUrl `T.isInfixOf`) urls
          where fetchUrl = view channelFetchUrl chan


logIt :: Text -> Text -> LF ()
logIt subject body = do
  widget <- view (lfWidgets . loggingWidget)
  liftIO . schedule $ ioLog widget subject body

logIt' :: Text -> LF ()
logIt' = join logIt

ioLog :: Widget (List Text FormattedText) -> Text -> Text -> IO ()
ioLog widget subject body = do
  t <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  let currTime = sformat hms t
  lbl <- plainText ("[" <> currTime <> "] " <> subject)
  addToList widget body lbl

getLogCommand :: LF (Text -> Text -> IO ())
getLogCommand = do
  widget <- view (lfWidgets . loggingWidget)
  return $ \subj body -> liftIO . schedule $ ioLog widget subj body

getStatusLogCommand :: LF (Text -> IO ())
getStatusLogCommand = do
  widget <- view (lfWidgets . statusBarWidget)
  return $ \status -> liftIO . schedule $
    setText widget status

statusSet :: Text -> LF ()
statusSet t = do
  w <- view (lfWidgets . statusBarWidget)
  liftIO $ schedule $ setText w t
