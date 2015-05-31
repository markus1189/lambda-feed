# λ-feed #

λ-feed - The Haskell feed reader

[![Build Status](https://travis-ci.org/markus1189/lambda-feed.png?branch=master)](https://travis-ci.org/markus1189/lambda-feed)

## About ##

A rss/atom feed reader written in Haskell that works on the
commandline.  Feeds are stored using **acid-state**.

This is a WIP and therefore a little rough around the edges.  In
addition to this note that it was primarily written for my personal
needs so it might not be everyone's case.  Having said that I
currently use it daily without major problems.

The intended (/my) workflow is to use it to quickly sort through your
favourite feeds and **bookmark** the items that interest you, where
**bookmark**ing means sending the item's url and title to an external
bookmarking service like *pocket* etc.  As a consequence rendering is
not a top priority, if you want to actually read items you will
probably not be happy with this program.
