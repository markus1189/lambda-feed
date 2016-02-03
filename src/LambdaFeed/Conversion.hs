{-# LANGUAGE CPP #-}
module LambdaFeed.Conversion (convertFeedToFeedItems, feedItems') where

import           Control.Applicative ((<|>))
import           Control.Lens (lazy, view, below, review,(#))
import           Control.Monad (guard,mplus)
import           Control.Monad (join)
import           Data.Digest.Pure.SHA (sha1, showDigest)
#if __GLASGOW_HASKELL__ >= 710
#else
import           Data.Functor ((<$>))
#endif
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Import as Atom
import qualified Text.Atom.Feed.Export as Atom
import           Text.Feed.Query
import           Text.Feed.Types as Ext
import           Text.Feed.Types as Feed
import           Text.RSS.Syntax as RSS
import           Text.RSS1.Syntax as RSS1
import           Text.XML.Light as XML


import           LambdaFeed.Types

convertFeedToFeedItems :: Text -> UTCTime -> Ext.Feed -> Seq FeedItem
convertFeedToFeedItems src now extFeed = convertFeedItem src now extFeed
                                     <$> Seq.fromList (feedItems' extFeed)

feedItems' :: Feed.Feed -> [Feed.Item]
feedItems' fe =
  case fe of
    AtomFeed f -> map Feed.AtomItem (Atom.feedEntries f)
    RSSFeed f  -> map Feed.RSSItem  (RSS.rssItems $ RSS.rssChannel f)
    RSS1Feed f -> map Feed.RSS1Item (RSS1.feedItems f)
    XMLFeed f -> case elementFeed' f of
                 Just f2 -> map Feed.AtomItem (Atom.feedEntries f2)
                 Nothing -> let items = map Feed.XMLItem $ XML.findElements (XML.unqual "item") f
                            in if not (null items)
                               then items
                               else map Feed.XMLItem $ XML.findElements (XML.unqual "entry") f

convertFeedItem :: Text -> UTCTime -> Ext.Feed -> Feed.Item -> FeedItem
convertFeedItem src now srcFeed i = FeedItem title url curl content pubDateOrNow chan guid
  where title = T.pack <$> getItemTitle i
        url = T.pack <$> getItemLink i
        curl = T.pack <$> getItemCommentLink i
        content = getFeedContent i
        guid = ((review _IdFromFeed . snd) <$> getItemId i)
           <|> (_IdFromLink #) <$> getItemLink i
           <|> sha
        chan = LambdaFeed.Types.Channel (T.pack (getFeedTitle srcFeed)) (T.pack <$> getFeedHome srcFeed) src
        pubDateOrNow = fromJust $ join (getItemPublishDate i) <|> Just now
        sha = review (below _IdFromContentSHA) $
                showDigest . sha1 . view lazy . T.encodeUtf8 <$> (content <> title)

getFeedContent :: Feed.Item -> Maybe Text
getFeedContent (AtomItem i) = case Atom.entryContent i of
  Just (Atom.HTMLContent c) -> Just (T.pack c)
  _ -> Nothing
getFeedContent i@(Feed.RSSItem _) = T.pack <$> getItemDescription i
getFeedContent _ = Nothing

elementFeed'  :: XML.Element -> Maybe Atom.Feed
elementFeed' e =
  do guard (elName e == Atom.atomName "feed")
     let es = Atom.children e
     i <- Atom.pLeaf "id" es
     t <- Atom.pTextContent "title" es `mplus` return (Atom.TextString "<no-title>")
     return Atom.Feed
       { Atom.feedId           = i
       , Atom.feedTitle        = t
       , Atom.feedSubtitle     = Atom.pTextContent "subtitle" es
       , Atom.feedUpdated      = ""
       , Atom.feedAuthors      = Atom.pMany "author" Atom.pPerson es
       , Atom.feedContributors = Atom.pMany "contributor" Atom.pPerson es
       , Atom.feedCategories   = Atom.pMany "category" Atom.pCategory es
       , Atom.feedGenerator    = Atom.pGenerator `fmap` Atom.pNode "generator" es
       , Atom.feedIcon         = Atom.pLeaf "icon" es
       , Atom.feedLogo         = Atom.pLeaf "logo" es
       , Atom.feedRights       = Atom.pTextContent "rights" es
       , Atom.feedLinks        = Atom.pMany "link" Atom.pLink es
       , Atom.feedEntries      = Atom.pMany "entry" Atom.pEntry es
       , Atom.feedOther        = other_es es
       , Atom.feedAttrs        = other_as (elAttribs e)
       }
  where
   other_es es = filter (\ el -> not (elName el `elem` known_elts))
                           es

   other_as as = filter (\ a -> not (attrKey a `elem` known_attrs))
                           as

    -- let's have them all (including xml:base and xml:lang + xmlns: stuff)
   known_attrs = []
   known_elts = map Atom.atomName
     [ "author"
     , "category"
     , "contributor"
     , "generator"
     , "icon"
     , "id"
     , "link"
     , "logo"
     , "rights"
     , "subtitle"
     , "title"
     , "updated"
     , "entry"
     ]
