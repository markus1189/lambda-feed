{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module LambdaFeed.Actor (Actor
                        ,actorInbox
                        ,actorOutbox
                        ,actorAsync

                        ,newActor
                        ,newActor'

                        ,killActor
                        ,isAlive
                        ) where

import Control.Concurrent.Async (Async,async,cancel,poll)
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad.State
#if __GLASGOW_HASKELL__ >= 710
#else
import Data.Functor ((<$>))
#endif
import Data.Maybe (isNothing)
import Pipes
import Pipes.Concurrent

data Actor a b = Actor {_actorInbox :: Output a
                       ,_actorOutbox :: Input b
                       ,_actorAsync :: Async ()
                       ,_actorSeal :: STM ()
                       }
makeLenses ''Actor

newActor :: Buffer a -> Buffer b -> Pipe a b IO () -> IO (Actor a b)
newActor = newActor' id

newActor' :: MonadIO m
          => (m () -> IO ())
          -> Buffer a
          -> Buffer b
          -> Pipe a b m ()
          -> IO (Actor a b)
newActor' runStack ba bb p = do
  (inboxOutput,inboxInput,seal1) <- spawn' ba
  (outboxOutput,outboxInput,seal2) <- spawn' bb
  ref <- async . runStack . runEffect $ fromInput inboxInput
                                    >-> p
                                    >-> toOutput outboxOutput
  return $ Actor inboxOutput outboxInput ref (seal1 >> seal2)

killActor :: Actor a b -> IO ()
killActor actor = atomically (actor ^. actorSeal) >> cancel (actor ^. actorAsync)

isAlive :: Actor a b -> IO Bool
isAlive (Actor _ _ t _) = isNothing <$> poll t
