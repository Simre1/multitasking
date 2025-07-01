module ConIO.Core where

-- ( -- ** Concurrent IO
--   ConIO,
--   runConIO,
--   runConIOCancel,

--   -- ** Task
--   Task,
--   launch,
--   AsyncThread (..),
--   cancelAll,

--   -- ** Manage scopes
--   ConScope,
--   withConScope,
--   useConScope,
--   UnsafeConScope,
--   toUnsafeConScope,
--   fromUnsafeConScope,

--   -- ** Exceptions
--   ConIOException (..),
--   ConIOKillThread (..),

--   -- ** Internal
--   Task (..),
-- )

import ConIO.MonadSTM
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (void)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Foldable (traverse_)
import Data.IORef
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import GHC.Records (HasField (..))
import Ki qualified

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

awaitAll :: (MonadSTM m) => Coordinator -> m ()
awaitAll (Coordinator scope) = liftSTM (Ki.awaitAll scope)

-- | A typeclass for all async workers which you can wait for.
class AsyncThread t where
  type Payload t

  -- | Wait for an async worker and return its payload.
  await :: (MonadSTM m) => t -> m (Payload t)

instance AsyncThread (Task a) where
  type Payload (Task a) = a
  await (Task thread) = liftSTM $ Ki.await thread
