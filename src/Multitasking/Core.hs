{-# LANGUAGE DefaultSignatures #-}

module Multitasking.Core
  ( -- ** Concurrency scopes
    multitask,
    Coordinator,

    -- ** Task
    Task (..),
    start,
    awaitTask,
    awaitAll,
  )
where

import Control.Monad.IO.Class
import Ki qualified
import Multitasking.AsyncOperations
import Multitasking.MonadSTM

-- | Coordinator keeps track of child processes
newtype Coordinator = Coordinator Ki.Scope

-- | Opens up a multitasking scope. No threads launched with the provided 'Coordinator' outlive this scope.
-- Before 'multitask' ends, it will __cancel all threads__. Use 'awaitAll' if you want to wait beforehand.
-- Additionally, exceptions between parent and children are propagated per default,
-- completely shutting down all processes when an exception happens anywhere.
--
-- You do not have to worry about:
--
-- - Zombie processes, since a thread can never outlive its parent scope.
-- - Dead processes, since exceptions will propagate to the parent thread.
multitask :: (MonadIO m) => (Coordinator -> IO a) -> m a
multitask f = liftIO $ Ki.scoped $ f . Coordinator

newtype Task a = Task (Ki.Thread a)

-- | 'start' is the main way to spin up new threads.
-- It will execute the given action in another thread and returns a 'Task'.
start :: (MonadIO m) => Coordinator -> IO a -> m (Task a)
start (Coordinator scope) action = do
  thread <- liftIO $ Ki.fork scope action
  pure $ Task thread

instance Functor Task where
  fmap f (Task t) = Task (fmap f t)

awaitTask :: (MonadSTM m) => Task a -> m a
awaitTask (Task thread) = liftSTM $ Ki.await thread

awaitAll :: (MonadSTM m) => Coordinator -> m ()
awaitAll (Coordinator scope) = liftSTM (Ki.awaitAll scope)

instance Await (Task a) where
  type Payload (Task a) = a
  await = awaitTask

instance Await Coordinator where
  type Payload Coordinator = ()
  await = awaitAll
