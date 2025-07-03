module Multitasking.Waiting
  ( -- ** Waiting
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

-- | Waits forever
waitForever :: (MonadIO m) => m a
waitForever = liftIO $ forever (threadDelay maxBound)

-- | Waits forever
waitDuration :: (MonadIO m) => Duration -> m ()
waitDuration duration = liftIO $ threadDelay $ fromIntegral $ durationToMicroseconds duration

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
