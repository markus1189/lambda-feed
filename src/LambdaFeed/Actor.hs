{-# LANGUAGE TemplateHaskell #-}
module LambdaFeed.Actor (Actor
                        ,actorInbox
                        ,actorOutbox
                        ,actorAsync

                        ,newActor
                        ,newActor'

                        ,killActor
                        ) where

import Control.Concurrent.Async (async,Async,cancel)
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad.State
import Pipes
import Pipes.Concurrent

data Actor a b = Actor {_actorInbox :: Output a
                       ,_actorOutbox :: Input b
                       ,_actorAsync :: Async ()
                       ,_actorSeal :: STM ()
                       }
makeLenses ''Actor

newActor :: Buffer a -> Buffer b -> Pipe a b IO () -> IO (Actor a b)
newActor ba bb p = newActor' ba bb p id

newActor' :: MonadIO m
          => Buffer a
          -> Buffer b
          -> Pipe a b m ()
          -> (m () -> IO ())
          -> IO (Actor a b)
newActor' ba bb p r = do
  (inboxOutput,inboxInput,seal1) <- spawn' ba
  (outboxOutput,outboxInput,seal2) <- spawn' bb
  ref <- async . r . runEffect $ fromInput inboxInput
                             >-> p
                             >-> toOutput outboxOutput
  return $ Actor inboxOutput outboxInput ref (seal1 >> seal2)

killActor :: Actor a b -> IO ()
killActor actor = atomically (actor ^. actorSeal) >> cancel (actor ^. actorAsync)
