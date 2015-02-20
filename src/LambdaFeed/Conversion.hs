module LambdaFeed.Conversion () where

convertFeedItem :: UTCTime -> Feed -> Item -> FeedItem
convertFeedItem now f i = FeedItem title url curl content pubDateOrNow chan
  where title = T.pack <$> (getItemTitle i)
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        chan = Channel (T.pack (getFeedTitle f)) (T.pack <$> (getFeedHome f))
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now
