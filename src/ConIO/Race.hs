{-# LANGUAGE RecursiveDo #-}

module ConIO.Race
  ( -- ** Race
    raceTwo,
    raceTwoMaybe,
    raceMany,
    raceManyMaybe,

    -- ** Timeout
    timeout,

    -- ** Waiting
    waitDuration,
    waitForever,

    -- ** Duration
    Duration,
    fromSeconds,
    fromMilliseconds,
    fromMicroseconds,
    durationToMicroseconds,
  )
where

import ConIO.Communication
import ConIO.Core
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Foldable (for_)

-- | Race two actions. The slower is canceled.
raceTwo :: (MonadIO m) => IO a -> IO a -> m a
raceTwo action1 action2 = multitask $ \inner -> do
  slot <- newSlot
  _ <- start inner $ action1 >>= fillSlot slot
  _ <- start inner $ action2 >>= fillSlot slot
  waitSlot slot

-- | Race two actions which may produce results. The slower is canceled.
raceTwoMaybe :: (MonadIO m) => IO (Maybe a) -> IO (Maybe a) -> m (Maybe a)
raceTwoMaybe action1 action2 = raceManyMaybe [action1, action2]

-- | Race many actions which may produce results. When one action produces a result, the other ones are cancelled.
raceMany :: (MonadIO m, Traversable t) => t (IO a) -> m a
raceMany actions = multitask $ \inner -> do
  slot <- newSlot
  for_ actions $ \action -> start inner $ action >>= fillSlot slot
  waitSlot slot

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
      Just a -> void $ fillSlot slot a

  atomically $ do
    maybeA <- tryReadSlot slot
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

-- | Waits forever
waitDuration :: (MonadIO m) => Duration -> m ()
waitDuration duration = liftIO $ threadDelay $ fromIntegral $ durationToMicroseconds duration

-- | Waits forever
waitForever :: (MonadIO m) => m a
waitForever = liftIO $ forever (threadDelay maxBound)

-- | 'Duration' is a time span. It is used for waiting and timeouts.
newtype Duration = Duration Word deriving (Show, Eq, Ord, Num)

-- | Create a 'Duration' from seconds
fromSeconds :: Word -> Duration
fromSeconds w = Duration $ w * 1000000

-- | Create a 'Duration' from milliseconds
fromMilliseconds :: Word -> Duration
fromMilliseconds w = Duration $ w * 1000

-- | Create a 'Duration' from microseconds
fromMicroseconds :: Word -> Duration
fromMicroseconds w = Duration w

-- Get the 'Duration' in microseconds.
durationToMicroseconds :: Duration -> Word
durationToMicroseconds (Duration w) = w
