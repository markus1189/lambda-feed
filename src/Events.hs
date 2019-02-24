module Events where

import Text.Feed.Types (Feed)

data CustomEvent = DownloadedFeed Feed deriving Show
