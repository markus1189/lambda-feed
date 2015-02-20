module LambdaFeed.Widgets (newArticleWidget
                          ,setArticle
                          ,saveSelection
                          ,myDefHighlight
                          ,myDefAttr
                          ,orange
                          ,myHeaderHighlight
                          ) where

import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import Control.Monad.IO.Class

newArticleWidget :: Show b => IO (Widget (List a b))
newArticleWidget = do
  lst <- newList 1
  setSelectedFocusedAttr lst (Just defAttr)
  lst `onKeyPressed` \this k _ -> case k of
    (KChar ' ') -> pageDown this >> return True
    KBS -> pageUp this >> return True
    _ -> return False
  return lst

setArticle :: Widget (List Text FormattedText) -> [(Text,Attr)] -> IO ()
setArticle w contents = do
  clearList w
  for_ contents $ \(content,attr) -> do
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

saveSelection :: MonadIO m => Widget (List a b) -> m () -> m ()
saveSelection w act = do
  maybeSelected <- liftIO (getSelected w)
  act
  for_ maybeSelected $ \(i,_) -> liftIO (setSelected w i)
