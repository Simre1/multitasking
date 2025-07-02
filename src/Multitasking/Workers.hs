module Multitasking.Workers
  ( -- ** Sink Task
    SinkTask,
    startSink,
    putSinkTask,

    -- ** Source Task
    SourceTask,
    startSource,
    takeSourceTask,
    drainSourceTask,

    -- ** Pipe Task
    PipeTask,
    startPipe,
    putPipeTask,
    takePipeTask,
    drainPipeTask,
  )
where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Void (Void)
import Multitasking.AsyncOperations
import Multitasking.Communication
import Multitasking.Core
import Multitasking.MonadSTM

-- | A 'SinkTask' continously consumes values of type `a`.
-- Multiples values can queue up if 'writeSinkTask' is too fast.
data SinkTask a = SinkTask
  { task :: Task Void,
    sink :: Sink a
  }

-- | start a 'SinkTask'. It will execute the given function until it is canceled.
startSink :: (MonadIO m) => Coordinator -> (a -> IO ()) -> m (SinkTask a)
startSink coordinator f = do
  queueA <- liftIO newQueue
  task <- start coordinator $ forever $ do
    a <- popQueue queueA
    f a

  pure $
    SinkTask
      { task,
        sink = queueSink queueA
      }

-- | Feeds a new value to the sink task
putSinkTask :: (MonadSTM m) => SinkTask a -> a -> m ()
putSinkTask sinkTask = putSink sinkTask.sink

instance Await (SinkTask a) where
  type Payload _ = Void
  await sinkTask = await sinkTask.task

-- | A 'SourceTask' is a task which continously produces values of type 'a'.
-- Multiples values can queue up to be consumed with 'readSourceTask'.
data SourceTask a = SourceTask
  { task :: Task Void,
    source :: Source a
  }

-- | start a 'SourceTask' with the given action. It will be executed forever to produce new values.
startSource :: (MonadIO m) => Coordinator -> IO a -> m (SourceTask a)
startSource coordinator f = liftIO $ do
  queueA <- newQueue
  task <- start coordinator $ forever $ do
    a <- f
    putQueue queueA a

  pure $
    SourceTask
      { task,
        source = queueSource queueA
      }

-- | Take a value from the 'SourceTask', waiting if there is no output.
takeSourceTask :: (MonadSTM m) => SourceTask a -> m a
takeSourceTask sourceTask = takeSource sourceTask.source

-- | Get a value from the 'SourceTask'. Returns immediately with 'Nothing' if there is no output.
drainSourceTask :: (MonadSTM m) => SourceTask a -> m (Maybe a)
drainSourceTask sourceTask = drainSource sourceTask.source

instance Await (SourceTask a) where
  type Payload _ = Void
  await sourceTask = await sourceTask.task

-- | A pipe task transforms values of type 'a' to values of type 'b'.
-- Both ends can queue up.
data PipeTask a b = PipeTask
  { task :: Task Void,
    sink :: Sink a,
    source :: Source b
  }

-- | start a 'PipeTask' with the given function. It will be executed continously.
startPipe :: (MonadIO m) => Coordinator -> (a -> IO b) -> m (PipeTask a b)
startPipe coordinator f = liftIO $ do
  queueA <- newQueue
  queueB <- newQueue
  task <- start coordinator $ forever $ do
    a <- popQueue queueA
    b <- f a
    putQueue queueB b

  let pipeTask =
        PipeTask
          { task,
            sink = queueSink queueA,
            source = queueSource queueB
          }

  pure $ pipeTask

-- | Feeds new values to the pipe task
putPipeTask :: (MonadSTM m) => PipeTask a b -> a -> m ()
putPipeTask pipeTask = putSink pipeTask.sink

-- | Gets the output from the pipe task. Waits if there is output available yet.
takePipeTask :: (MonadSTM m) => PipeTask a b -> m b
takePipeTask pipeTask = takeSource pipeTask.source

-- | Gets the output from the pipe task. Does not wait for output.
drainPipeTask :: (MonadSTM m) => PipeTask a b -> m (Maybe b)
drainPipeTask pipeTask = drainSource pipeTask.source

instance Await (PipeTask a b) where
  type Payload _ = Void
  await pipeTask = await pipeTask.task
