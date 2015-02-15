module LambdaFeed.Widgets (newArticleWidget
                          ,setArticle
                          ) where

import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Graphics.Vty
import           Graphics.Vty.Widgets.All

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
