{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Multitasking.Communication where

import Control.Concurrent.Multitasking.Task
import Control.Concurrent.STM
import Control.Monad.IO.Class

data AsyncResult a = SingleValue a

asyncSTM :: (MonadTask m) => STM a -> m a
asyncSTM stm = async $ liftIO (atomically stm)

class BlockingRead container where
  type ReadValue container
  blockingRead :: container -> STM (ReadValue container)

asyncRead :: (BlockingRead container, MonadTask m) => container -> m (ReadValue container)
asyncRead = asyncSTM . blockingRead

class (BlockingRead container) => BlockingTake container where
  blockingTake :: container -> STM (ReadValue container)

asyncTake :: (BlockingTake container, MonadTask m) => container -> m (ReadValue container)
asyncTake = asyncSTM . blockingTake

class BlockingWrite container where
  type WriteValue container
  blockingWrite :: container -> WriteValue container -> STM ()

class BlockingPut container where
  blockingPut :: container -> WriteValue container -> STM ()
