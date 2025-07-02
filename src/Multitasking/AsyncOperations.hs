{-# LANGUAGE DefaultSignatures #-}

module Multitasking.AsyncOperations where

import Control.Concurrent.STM
import Multitasking.MonadSTM

class Await t where
  type Payload t

  -- | Wait for the value
  await :: (MonadSTM m) => t -> m (Payload t)

  -- | Probe for the value
  probe :: (MonadSTM m) => t -> m (Maybe (Payload t))

  {-# MINIMAL await | probe #-}

  default await :: (MonadSTM m) => t -> m (Payload t)
  await t = liftSTM $ do
    maybePayload <- probe t
    case maybePayload of
      Just a -> pure a
      Nothing -> retry
  default probe :: (MonadSTM m) => t -> m (Maybe (Payload t))
  probe t = liftSTM $ (Just <$> await t) `orElse` pure Nothing
