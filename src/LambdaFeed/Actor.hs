{-# LANGUAGE TemplateHaskell #-}
module LambdaFeed.Actor (Actor
                        ,actorInbox
                        ,actorOutbox
                        ,actorAsync

                        ,newActor
                        ,newActor'

                        ,killActor
                        ,link
                        ) where

import Control.Concurrent.Async (async,Async,cancel,link2)
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

newActor :: Pipe a b IO () -> IO (Actor a b)
newActor p = newActor' p id

newActor' :: MonadIO m => Pipe a b m () -> (m () -> IO ()) -> IO (Actor a b)
newActor' p r = do
  (inboxOutput,inboxInput,seal1) <- spawn' unbounded
  (outboxOutput,outboxInput,seal2) <- spawn' unbounded
  ref <- async . r . runEffect $ fromInput inboxInput
                             >-> p
                             >-> toOutput outboxOutput
  return $ Actor inboxOutput outboxInput ref (seal1 >> seal2)

killActor :: Actor a b -> IO ()
killActor actor = atomically (actor ^. actorSeal) >> cancel (actor ^. actorAsync)

link :: Actor a b -> Actor c d -> IO ()
link (Actor _ _ r1 _) (Actor _ _ r2 _) = link2 r1 r2
