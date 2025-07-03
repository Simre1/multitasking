{-# LANGUAGE RecursiveDo #-}

module Multitasking.Race
  ( -- ** Race
    raceTwo,
    raceTwoMaybe,
    raceMany,
    raceManyMaybe,

    -- ** Timeout
    timeout,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Multitasking.Communication
import Multitasking.Core
import Multitasking.Waiting

-- | Race two actions. The slower one is canceled.
raceTwo :: (MonadIO m) => IO a -> IO a -> m a
raceTwo action1 action2 = multitask $ \inner -> do
  slot <- newSlot
  _ <- start inner $ action1 >>= putSlot slot
  _ <- start inner $ action2 >>= putSlot slot
  awaitSlot slot

-- | Race two actions which may produce results. The slower one is canceled.
raceTwoMaybe :: (MonadIO m) => IO (Maybe a) -> IO (Maybe a) -> m (Maybe a)
raceTwoMaybe action1 action2 = raceManyMaybe [action1, action2]

-- | Race many actions which may produce results. When one action produces a result, the other ones are cancelled.
raceMany :: (MonadIO m, Traversable t) => t (IO a) -> m a
raceMany actions = multitask $ \inner -> do
  slot <- newSlot
  for_ actions $ \action -> start inner $ action >>= putSlot slot
  awaitSlot slot

-- | Race many actions. The slower ones are canceled.
-- If no actions produce a result, then the resulting task will also produce nothing.
raceManyMaybe :: (MonadIO m, Traversable t) => t (IO (Maybe a)) -> m (Maybe a)
raceManyMaybe actions = multitask $ \coordinator -> do
  counter <- newCounter (length actions)
  slot <- newSlot
  for_ actions $ \action -> start coordinator $ do
    maybeA <- action
    decrementCounter counter
    case maybeA of
      Nothing -> pure ()
      Just a -> void $ putSlot slot a

  atomically $ do
    maybeA <- probeSlot slot
    case maybeA of
      Just a -> pure $ Just a
      Nothing -> do
        finished <- getCounter counter
        if finished == 0
          then pure Nothing
          else retry

-- | Time out an action.
timeout :: (MonadIO m) => Duration -> IO a -> m (Maybe a)
timeout duration action =
  raceTwo (Just <$> action) (threadDelay maxWaitTime >> pure Nothing)
  where
    maxWaitTime = fromIntegral $ durationToMicroseconds duration
