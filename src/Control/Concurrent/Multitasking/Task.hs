module Control.Concurrent.Multitasking.Task
  ( async,
    multitask,
    MonadTask (..),
    TaskT (..),
    Task,
    runTaskT,
    Coordinator,
    withThreadOptions,
    modifyThreadOptions,
    Ki.ThreadOptions (..),
    Ki.ThreadAffinity (..),
    Ki.ByteCount,
    Ki.kilobytes,
    Ki.megabytes,
  )
where

import Control.Concurrent
import Control.Concurrent.Multitasking.Coordinator
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Ki.Unlifted qualified as Ki
import System.IO.Unsafe
import UnliftIO (MonadUnliftIO)

newtype TaskT m a = TaskT (ReaderT Coordinator m a) deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadUnliftIO)

type Task = TaskT IO

class (MonadUnliftIO m) => MonadTask m where
  askCoordinator :: m Coordinator
  localCoordinator :: (Coordinator -> Coordinator) -> m a -> m a

instance (MonadUnliftIO m) => MonadTask (TaskT m) where
  askCoordinator = TaskT ask
  localCoordinator f (TaskT m) = TaskT $ local f m

runTaskT :: (MonadUnliftIO m) => TaskT m a -> m a
runTaskT (TaskT m) = do
  Ki.scoped
    ( \scope -> do
        x <- runReaderT m (Coordinator {scope, options = Ki.defaultThreadOptions})
        liftIO $ atomically $ Ki.awaitAll scope
        pure x
    )

multitask :: (MonadTask m) => m a -> m a
multitask m = Ki.scoped $ \scope -> do
  a <- localCoordinator (\c -> c {scope = scope}) m
  liftIO $ atomically $ Ki.awaitAll scope
  pure a

async :: (MonadTask m) => m a -> m a
async action = do
  Coordinator {scope, options} <- askCoordinator
  thread <- Ki.forkWith scope options action
  liftIO $ unsafeDupableInterleaveIO $ atomically $ Ki.await thread

withThreadOptions :: (MonadTask m) => (Ki.ThreadOptions -> Ki.ThreadOptions) -> m a -> m a
withThreadOptions f = localCoordinator (modifyThreadOptions f)
