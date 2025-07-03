{-# LANGUAGE DefaultSignatures #-}

module Multitasking.Core
  ( -- ** Start tasks
    multitask,
    Coordinator,
    start,
    Task (..),
    awaitTask,
    awaitAll,

    -- ** Control thread options
    startWith,
    Ki.ThreadOptions (..),
    Ki.defaultThreadOptions,
    Ki.ThreadAffinity (..),
    Ki.ByteCount,
    Ki.kilobytes,
    Ki.megabytes,
  )
where

import Control.Monad.IO.Class
import Ki qualified
import Multitasking.AsyncOperations
import Multitasking.MonadSTM

-- | Coordinator corresponds to the current multitasking scope.
newtype Coordinator = Coordinator Ki.Scope

-- | Opens up a multitasking scope. No threads launched with the provided 'Coordinator' outlive this scope.
-- Before 'multitask' ends, it will __cancel all threads__. Use 'awaitAll' if you want to wait beforehand.
-- Additionally, exceptions between parent and children are propagated per default,
-- completely shutting down all processes when an exception happens anywhere.
multitask :: (MonadIO m) => (Coordinator -> IO a) -> m a
multitask f = liftIO $ Ki.scoped $ f . Coordinator

-- | A 'Task' is a computation on another thread. Use `await` to wait for the task.
newtype Task a = Task (Ki.Thread a)

-- | 'start' is the main way to spin up new tasks.
-- It will execute the given action in another thread and returns a 'Task'.
start :: (MonadIO m) => Coordinator -> IO a -> m (Task a)
start (Coordinator scope) action = do
  thread <- liftIO $ Ki.fork scope action
  pure $ Task thread

-- | Provice
startWith :: (MonadIO m) => Coordinator -> Ki.ThreadOptions -> IO a -> m (Task a)
startWith (Coordinator scope) to action = do
  thread <- liftIO $ Ki.forkWith scope to action
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
