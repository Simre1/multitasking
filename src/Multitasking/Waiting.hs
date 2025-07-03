module Multitasking.Waiting
  ( -- ** Waiting
    waitDuration,
    waitForever,

    -- ** Duration
    Duration,
    fromMinutes,
    fromWholeMinutes,
    fromSeconds,
    fromWholeSeconds,
    fromMilliseconds,
    fromWholeMilliseconds,
    fromMicroseconds,
    durationToMicroseconds,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class

-- | Waits forever
waitForever :: (MonadIO m) => m a
waitForever = liftIO $ forever (threadDelay maxBound)

-- | Waits forever
waitDuration :: (MonadIO m) => Duration -> m ()
waitDuration duration = liftIO $ threadDelay $ fromIntegral $ durationToMicroseconds duration

-- | 'Duration' is a time span. It is used for waiting and timeouts.
newtype Duration = Duration Word deriving (Show, Eq, Ord, Num)

-- | Create a 'Duration' from minutes
fromMinutes :: Double -> Duration
fromMinutes d = fromSeconds (d * 60)

-- | Create a 'Duration' from seconds
fromSeconds :: Double -> Duration
fromSeconds d = Duration $ round $ d * 1000000

-- | Create a 'Duration' from milliseconds
fromMilliseconds :: Double -> Duration
fromMilliseconds d = Duration $ round $ d * 1000

-- | Create a 'Duration' from minutes
fromWholeMinutes :: Word -> Duration
fromWholeMinutes w = fromWholeSeconds (w * 60)

-- | Create a 'Duration' from seconds
fromWholeSeconds :: Word -> Duration
fromWholeSeconds w = Duration $ w * 1000000

-- | Create a 'Duration' from milliseconds
fromWholeMilliseconds :: Word -> Duration
fromWholeMilliseconds w = Duration $ w * 1000

-- | Create a 'Duration' from microseconds
fromMicroseconds :: Word -> Duration
fromMicroseconds w = Duration w

-- Get the 'Duration' in microseconds.
durationToMicroseconds :: Duration -> Word
durationToMicroseconds (Duration w) = w
