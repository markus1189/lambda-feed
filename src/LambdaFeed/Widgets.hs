{-# LANGUAGE CPP #-}
module LambdaFeed.Widgets (newArticleWidget
                          ,setArticle
                          ,saveSelection
                          ,myDefHighlight
                          ,myDefAttr
                          ,orange
                          ,myHeaderHighlight
                          ,getListItems
                          ) where

import           Data.Maybe (catMaybes)
import           Control.Monad.IO.Class
import           Data.Foldable (for_,)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Graphics.Vty
import           Graphics.Vty.Widgets.All

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif

newArticleWidget :: Show b => IO (Widget (List a b))
newArticleWidget = do
  lst <- newList 1
  setSelectedFocusedAttr lst (Just defAttr)
  setSelectedUnfocusedAttr lst (Just defAttr)
  lst `onKeyPressed` \this k _ -> case k of
    (KChar ' ') -> pageDown this >> return True
    KBS -> pageUp this >> return True
    _ -> return False
  return lst

setArticle :: Widget (List Text FormattedText) -> [(Text,Attr)] -> IO ()
setArticle w contents = do
  clearList w
  for_ contents $ \(content,attr) ->
    for_ (T.lines content)  $ \line -> do
      lineW <- plainTextWithAttrs [(line,attr)]
      addToList w line lineW

myDefHighlight :: Attr
myDefHighlight = black' `on` orange `withStyle` bold
  where black' = rgbColor (0 :: Int) 0 0

myDefAttr :: Attr
myDefAttr = defAttr `withForeColor` white

orange :: Color
orange = rgbColor 215 135 (0 :: Int)

myHeaderHighlight :: Attr
myHeaderHighlight = myDefAttr `withForeColor` orange `withStyle` bold

saveSelection :: (Applicative m, MonadIO m) => Widget (List a b) -> m () -> m ()
saveSelection w act = do
  maybeSelected <- liftIO (getSelected w)
  act
  for_ maybeSelected $ \(i,_) -> liftIO (setSelected w i)

getListItems :: Widget (List a b) -> IO [a]
getListItems w = do
  size <- getListSize w
  map fst . catMaybes <$> for [0..size-1] (getListItem w)
