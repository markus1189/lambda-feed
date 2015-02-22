{-# LANGUAGE OverloadedStrings #-}
module LambdaFeed (start) where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (STM)
import           Control.Exception (try, SomeException)
import           Control.Exception.Extra (retry)
import           Control.Lens (view, non, use, below, lazy, from, review)
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Data.Acid.Advanced (query', update')
import           Data.Digest.Pure.SHA
import           Data.Foldable
import           Data.List (sortBy, findIndex)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Data.Sequence ((><), Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Lens (utf8, packed)
import           Data.Text.Lens (_Text)
import           Data.Time (UTCTime, utcToLocalTime)
import           Data.Time (formatTime, defaultTimeLocale, rfc822DateFormat)
import           Data.Time (getCurrentTime, getCurrentTimeZone)
import           Formatting (sformat, left, (%), (%.), stext, int)
import           Formatting.Time (monthNameShort, dayOfMonth, hms)
import           Graphics.Vty (Attr)
import           Graphics.Vty.Widgets.All (Widget, List, FormattedText, clearList, plainText, addToList, getSelected, setText, insertIntoList)
import           Graphics.Vty.Widgets.EventLoop (schedule, shutdownUi)
import           Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import           Pipes
import           Pipes.Concurrent (fromInput, atomically, Input)
import           System.Exit (exitSuccess)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (readProcess, rawSystem)
import qualified Text.Atom.Feed as Atom
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types

import           LambdaFeed.Types
import           LambdaFeed.Widgets

showChannels :: LF ()
showChannels = view (lfSwitch . switchToChannels) >>= (liftIO . schedule)

getFeedContent :: Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing

showItemsFor :: Channel -> LF ()
showItemsFor chan = do
  lfCurrentChannel ?= chan
  widget <- view (lfWidgets . itemWidget)
  acid <- view lfAcid
  vis <- use lfVisibility
  items <- query' acid (GetItems vis chan)
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

fetchAllFeeds :: LF ()
fetchAllFeeds = do
  statusbar <- view (lfWidgets . statusBarWidget)
  acid <- view lfAcid
  feedsToFetch <- view lfUrls
  liftIO $ do
    schedule $ setText statusbar "Updating..."
    void . forkIO $ do
      feeds <- fetchN feedsToFetch
      update' acid (UpdateFeeds feeds)
      schedule $ setText statusbar "Done."

explode :: UTCTime -> Feed -> Seq FeedItem
explode now f = fmap (convertFeedItem now f) (Seq.fromList $ getFeedItems f)

convertFeedItem :: UTCTime -> Feed -> Item -> FeedItem
convertFeedItem now f i = FeedItem title url curl content pubDateOrNow chan guid
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        guid = (review _IdFromFeed . snd) <$> getItemId i <|> sha
        chan = Channel (T.pack (getFeedTitle f)) (T.pack <$> (getFeedHome f))
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now
        sha = review (below _IdFromContentSHA) $ showDigest . sha1 . view lazy . T.encodeUtf8 <$> (content <> title)

fetchN :: [Text] -> IO (Seq FeedItem)
fetchN urls = getCurrentTime >>= \t -> foldM (go t) Seq.empty urls
  where go now acc url = maybe acc (\feed -> acc >< (explode now feed)) <$> fetch1 url

try' :: IO a -> IO (Either SomeException a)
try' = try

fetch1 :: Text -> IO (Maybe Feed)
fetch1 url = do
  feedOrErr <- try' (Wreq.get (T.unpack url))
  case feedOrErr of
    Left _ -> return Nothing
    Right feed -> return . parseFeedString . view (responseBody . utf8 . from packed) $ feed

guiEventHandler :: STM () -> Consumer GuiEvent LF ()
guiEventHandler seal = forever $ await >>= lift . handle
  where handle :: GuiEvent -> LF ()
        handle FetchAll = do
          logIt' "Started update"
          fetchAllFeeds
          updateChannelWidget
        handle ShowChannels = showChannels
        handle (ChannelActivated chan) = showItemsFor chan
        handle (ItemActivated item) = showContentFor item
        handle QuitLambdaFeed = liftIO $ atomically seal >> shutdownUi >> exitSuccess
        handle BackToChannels = do
          updateChannelWidget
          view (lfSwitch . switchToChannels) >>= liftIO . schedule
        handle BackToItems = view (lfSwitch . switchToItems) >>= liftIO . schedule
        handle MarkChannelRead = markChannelRead
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
        handle SwitchToLogging = view (lfSwitch . switchToLogging) >>= liftIO . schedule

markChannelRead :: LF ()
markChannelRead = do
  widget <- view (lfWidgets . channelWidget)
  maybeSel <- liftIO $ getSelected widget
  acid <- view lfAcid
  for_ maybeSel $ \(_,(chan,_)) -> do
    update' acid (MarkAsRead chan)
    updateChannelWidget

executeExternal :: FeedItem -> LF ()
executeExternal item = do
  logCmd <- getLogCommand
  statusLogCmd <- getStatusLogCommand
  (command,args) <- view lfExternalCommand
  let maybeUrlTitle = (,) <$> (view itemCommentUrl item <|> view itemUrl item) <*> view itemTitle item
  for_ maybeUrlTitle $ \(url,title) -> do
    logIt' $ T.pack command <> " " <> url <> " " <> title
    liftIO . forkIO $ do
      res <- try' . retry 3 $
               rawSystem command $ args ++ [(T.unpack url), (T.unpack title)]
      case res of
        Left e -> do
          statusLogCmd "External command failed (see log)."
          logCmd "Command failed" (T.pack . show $ e)
        Right _ -> return ()

updateChannelWidget :: LF ()
updateChannelWidget = do
  widget <- view (lfWidgets . channelWidget)
  acid <- view lfAcid
  vis <- use lfVisibility
  (unreadItems,readItems) <- query' acid AllItems
  visibleChannels <- query' acid (GetChannels vis) >>= sortAsGiven
  saveSelection widget . liftIO . schedule $ do
    clearList widget
    for_ visibleChannels $ \chan -> do
      let numItemsUnread = maybe 0 Seq.length $ Map.lookup chan unreadItems
          numItemsRead = maybe 0 Seq.length $ Map.lookup chan readItems
          total = numItemsRead + numItemsUnread
          fmtTotal = sformat ("(" % int % "/" % int % ")") numItemsUnread total
      lbl <- liftIO $ plainText $ sformat ((left 11 ' ' %. stext) % " " % stext)
                                          fmtTotal (view channelTitle chan)
      addToList widget chan lbl

sortAsGiven :: Foldable f => f Channel -> LF [Channel]
sortAsGiven cs = do
  urls <- view lfUrls
  return $ sortBy (comparing (indexAsGiven urls)) (toList cs)
  where indexAsGiven us chan = do
          cUrl <- view channelUrl chan
          findIndex (cUrl `T.isInfixOf`) us

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
  insertIntoList widget body lbl 0

getLogCommand :: LF (Text -> Text -> IO ())
getLogCommand = do
  widget <- view (lfWidgets . loggingWidget)
  return $ \subj body -> liftIO . schedule $ ioLog widget subj body

getStatusLogCommand :: LF (Text -> IO ())
getStatusLogCommand = do
  widget <- view (lfWidgets . statusBarWidget)
  return $ \status -> liftIO . schedule $ do
    setText widget status
