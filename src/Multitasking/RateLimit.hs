module Multitasking.RateLimit
  ( -- ** Throttling
    RateLimit,
    throttle,
    maxConcurrentTasks,
    tokenBucket,
  )
where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, when)
import Control.Monad.IO.Class
import Multitasking.Communication
import Multitasking.Core
import Multitasking.MonadSTM
import Multitasking.Waiting

-- | Specifies a rate limit which can be applied to actions
newtype RateLimit = RateLimit (forall a. IO a -> IO a)

-- | Delay the given action according to the 'RateLimit'.
throttle :: RateLimit -> IO a -> IO a
throttle (RateLimit f) = f

-- | Limits concurrency to exactly N tasks. If N tasks are already running, the next one needs to wait.
maxConcurrentTasks :: (MonadSTM m) => Int -> m RateLimit
maxConcurrentTasks concurrency = do
  counter <- newCounter concurrency
  pure $ RateLimit $ \action -> do
    atomically $ do
      value <- getCounter counter
      check (value > 0)
      decrementCounter counter
    action `finally` incrementCounter counter

-- | Limits concurrency to a specific rate with the tocket bucket strategy.
tokenBucket ::
  (MonadIO m) =>
  Coordinator ->
  -- | Specify the rate as a 'Duration':  @fromSeconds (1 / X)@ for X tasks per second
  Duration ->
  -- | Allowed burst tasks: X burst tasks means that X tasks can start without waiting
  Int ->
  m RateLimit
tokenBucket coordinator recharge burst' = liftIO $ do
  let burst = min burst' 1
  counter <- newCounter burst
  _ <- start coordinator $ forever $ do
    waitDuration recharge
    atomically $ do
      value <- getCounter counter
      when (value < burst) (incrementCounter counter)
  pure $ RateLimit $ \action -> do
    atomically $ do
      value <- getCounter counter
      check (value > 0)
      decrementCounter counter
    action
