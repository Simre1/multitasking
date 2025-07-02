{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Multitasking.Communication
  ( -- ** Variable
    Variable (..),
    newVariable,
    readVariable,
    writeVariable,

    -- ** Option
    Option (..),
    newOption,
    newEmptyOption,
    writeOption,
    putOption,
    offerOption,
    awaitOption,
    probeOption,
    takeOption,
    drainOption,

    -- ** Gate
    Gate (..),
    newGate,
    awaitGate,
    openGate,

    -- ** Switch
    Switch (..),
    newSwitch,
    awaitSwitch,
    setSwitch,
    unsetSwitch,
    toggleSwitch,

    -- ** Condition,
    Condition (..),
    newCondition,
    awaitCondition,
    probeCondition,

    -- ** Slot
    Slot (..),
    newSlot,
    putSlot,
    awaitSlot,
    probeSlot,

    -- ** Counter
    Counter (..),
    newCounter,
    getCounter,
    awaitCounter,
    incrementCounter,
    decrementCounter,

    -- ** Sink
    Sink,
    newSink,
    putSink,

    -- ** Source
    Source,
    newSource,
    takeSource,
    drainSource,

    -- ** Queue
    Queue (..),
    newQueue,
    popQueue,
    peekQueue,
    putQueue,
    isEmptyQueue,
    queueSource,
    queueSink,
  )
where

import Control.Concurrent.STM
import Data.Functor (($>))
import Data.Functor.Contravariant
import Multitasking.MonadSTM

-- | A 'Gate' is initially closed and can be opened with 'openGate'.
newtype Gate = Gate (TMVar ())

newGate :: (MonadSTM m) => m Gate
newGate = Gate <$> liftSTM_IO newEmptyTMVar newEmptyTMVarIO

-- | Open a 'Gate'. You __cannot__ close a 'Gate'.
openGate :: (MonadSTM m) => Gate -> m ()
openGate (Gate tmvar) = liftSTM $ writeTMVar tmvar ()

-- | Wait for the 'Gate' to open.
awaitGate :: (MonadSTM m) => Gate -> m ()
awaitGate (Gate tmvar) = liftSTM $ readTMVar tmvar

-- | A 'Switch' is either on or off.
newtype Switch = Switch (TVar Bool)

newSwitch :: (MonadSTM m) => Bool -> m Switch
newSwitch b = Switch <$> liftSTM_IO (newTVar b) (newTVarIO b)

-- | Wait until the 'Switch' is on
awaitSwitch :: (MonadSTM m) => Switch -> m ()
awaitSwitch (Switch tvar) = liftSTM $ do
  result <- readTVar tvar
  check result

-- | Turn on the 'Switch'
setSwitch :: (MonadSTM m) => Switch -> m ()
setSwitch (Switch tVar) = liftSTM $ writeTVar tVar True

-- | Turn off the 'Switch'
unsetSwitch :: (MonadSTM m) => Switch -> m ()
unsetSwitch (Switch tVar) = liftSTM $ writeTVar tVar False

-- | Toggle the 'Switch' between on/off.
toggleSwitch :: (MonadSTM m) => Switch -> m ()
toggleSwitch (Switch tVar) = liftSTM $ do
  state <- readTVar tVar
  if state
    then writeTVar tVar False
    else writeTVar tVar True

-- | A 'Variable' holds some value in a concurrency-safe, non-blocking manner.
newtype Variable a = Variable (TVar a)

newVariable :: (MonadSTM m) => a -> m (Variable a)
newVariable a = Variable <$> liftSTM_IO (newTVar a) (newTVarIO a)

-- | Read in a non-blocking manner
readVariable :: (MonadSTM m) => Variable a -> m a
readVariable (Variable tvar) = liftSTM $ readTVar tvar

-- | Write in a non-blocking manner
writeVariable :: (MonadSTM m) => Variable a -> a -> m ()
writeVariable (Variable tvar) a = liftSTM $ writeTVar tvar a

-- | A 'Condition' puts a constraint on a value that you can wait for
data Condition a = Condition (a -> Bool) (STM a)

newCondition :: (a -> Bool) -> STM a -> Condition a
newCondition f stm = Condition f stm

-- | Wait for the 'Condition' to be true
awaitCondition :: (MonadSTM m) => Condition a -> m a
awaitCondition (Condition f stm) = liftSTM $ do
  a <- stm
  check (f a)
  pure a

-- | Probe the 'Condition' status
probeCondition :: (MonadSTM m) => Condition a -> m (Maybe a)
probeCondition (Condition f stm) =
  liftSTM $
    (stm >>= \a -> check (f a) $> Just a) `orElse` pure Nothing

-- | A 'Counter' stores an int.
newtype Counter = Counter (TVar Int)

-- | Create a new 'Counter' with an initial value.
newCounter :: (MonadSTM m) => Int -> m Counter
newCounter initial = Counter <$> liftSTM_IO (newTVar initial) (newTVarIO initial)

-- | Get the current value of the 'Counter'.
getCounter :: (MonadSTM m) => Counter -> m Int
getCounter (Counter tVar) = liftSTM $ readTVar tVar

-- | Wait until the 'Counter' has the value
awaitCounter :: (MonadSTM m) => Counter -> Int -> m ()
awaitCounter (Counter tVar) i = liftSTM $ do
  value <- readTVar tVar
  check (value == i)

-- | Increment the 'Counter' by one.
incrementCounter :: (MonadSTM m) => Counter -> m ()
incrementCounter (Counter tVar) = liftSTM $ modifyTVar' tVar succ

-- | Decrement the 'Counter' by one.
decrementCounter :: (MonadSTM m) => Counter -> m ()
decrementCounter (Counter tVar) = liftSTM $ modifyTVar' tVar pred

-- | An 'Option' is either empty or contains an `a`.
newtype Option a = Option (TMVar a)

-- | Creates a new 'Option' filled with `a`.
newOption :: (MonadSTM m) => a -> m (Option a)
newOption a = Option <$> liftSTM_IO (newTMVar a) (newTMVarIO a)

-- | Creates a new empty 'Option'.
newEmptyOption :: (MonadSTM m) => m (Option a)
newEmptyOption = Option <$> liftSTM_IO newEmptyTMVar newEmptyTMVarIO

-- | Write in non-blocking manner
writeOption :: (MonadSTM m) => Option a -> a -> m ()
writeOption (Option tmvar) a = liftSTM $ writeTMVar tmvar a

-- | Puts in a blocking manner, waiting if the 'Option' is not empty.
putOption :: (MonadSTM m) => Option a -> a -> m ()
putOption (Option tmvar) a = liftSTM $ putTMVar tmvar a

-- | Offers a value to the 'Option' in a non-blocking manner. Returns whether the offer was accepted or not.
offerOption :: (MonadSTM m) => Option a -> a -> m Bool
offerOption (Option tmvar) a = liftSTM $ tryPutTMVar tmvar a

-- | Awaits the value from the 'Option'.
awaitOption :: (MonadSTM m) => Option a -> m a
awaitOption (Option var) = liftSTM (readTMVar var)

-- | Probes the 'Option' in a non-blocking manner.
probeOption :: (MonadSTM m) => Option a -> m (Maybe a)
probeOption (Option var) = liftSTM (tryReadTMVar var)

-- | Takes the element from the 'Option'. Waits if there is no element.
-- Afterwards, the 'Option' is empty.
takeOption :: (MonadSTM m) => Option a -> m a
takeOption (Option var) = liftSTM (takeTMVar var)

-- | Tries to take the element from the 'Option' in a non-blocking manner.
-- Afterwards, the 'Option' is empty.
drainOption :: (MonadSTM m) => Option a -> m (Maybe a)
drainOption (Option var) = liftSTM (tryTakeTMVar var)

-- | A 'Queue' holds zero or more values.
newtype Queue a = Queue (TChan a)

newQueue :: (MonadSTM m) => m (Queue a)
newQueue = Queue <$> liftSTM_IO newTChan newTChanIO

-- | Pop the first element of the 'Queue', removing it from the queue.
-- Waits until an element is available.
popQueue :: (MonadSTM m) => Queue a -> m a
popQueue (Queue chan) = liftSTM $ readTChan chan

-- | Get the first element of the 'Queue', __not__ removing it from the queue.
-- Waits until an element is available.
peekQueue :: (MonadSTM m) => Queue a -> m a
peekQueue (Queue chan) = liftSTM $ peekTChan chan

-- | Push an element to the back of the 'Queue'.
putQueue :: (MonadSTM m) => Queue a -> a -> m ()
putQueue (Queue chan) a = liftSTM $ writeTChan chan a

-- | Checks if the 'Queue' is empty.
isEmptyQueue :: (MonadSTM m) => Queue a -> m Bool
isEmptyQueue (Queue chan) = liftSTM $ isEmptyTChan chan

queueSource :: Queue a -> Source a
queueSource queue = newSource $ popQueue queue

queueSink :: Queue a -> Sink a
queueSink queue = newSink $ putQueue queue

-- | A 'Sink' represents a channel where you can only push data
newtype Sink a = Sink (a -> STM ())

instance Contravariant Sink where
  contramap f (Sink s) = Sink (s . f)

-- | A 'Source' represents a channel where you can only read data
newtype Source a = Source (STM a) deriving (Functor, Applicative)

newSource :: STM a -> Source a
newSource = Source

takeSource :: (MonadSTM m) => Source a -> m a
takeSource (Source stm) = liftSTM stm

drainSource :: (MonadSTM m) => Source a -> m (Maybe a)
drainSource (Source stm) = liftSTM $ orElse (Just <$> stm) (pure Nothing)

newSink :: (a -> STM ()) -> Sink a
newSink = Sink

putSink :: (MonadSTM m) => Sink a -> a -> m ()
putSink (Sink f) a = liftSTM $ f a

-- | A 'Slot' starts out empty and can be filled with 'putSlot'.
newtype Slot a = Slot (TMVar a)

newSlot :: (MonadSTM m) => m (Slot a)
newSlot = Slot <$> liftSTM_IO newEmptyTMVar newEmptyTMVarIO

putSlot :: (MonadSTM m) => Slot a -> a -> m Bool
putSlot (Slot var) a = liftSTM $ tryPutTMVar var a

awaitSlot :: (MonadSTM m) => Slot a -> m a
awaitSlot (Slot var) = liftSTM $ readTMVar var

probeSlot :: (MonadSTM m) => Slot a -> m (Maybe a)
probeSlot (Slot var) = liftSTM $ tryReadTMVar var
