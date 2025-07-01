module ConIO.Workers where

--   ( -- ** Sink Task
--     SinkTask,
--     startSink,
--     writeSinkTask,

--     -- ** Source Task
--     SourceTask,
--     startSource,
--     readSourceTask,
--     tryReadSourceTask,

--     -- ** Pipe Task
--     PipeTask,
--     startPipe,
--     feedPipeTask,
--     getPipeTask,
--     tryGetPipeTask,
--   )
-- where

-- import ConIO.Communication
-- import ConIO.Core
-- import ConIO.MonadSTM
-- import Control.Monad (forever)
-- import Data.Void (Void)

-- -- | A 'SinkTask' continously consumes values of type `a`.
-- -- Multiples values can queue up if 'writeSinkTask' is too fast.
-- data SinkTask a = SinkTask
--   { task :: Task Void,
--     sink :: Sink a
--   }

-- -- | start a 'SinkTask'. It will execute the given function until it is canceled.
-- startSink :: Coordinator -> (a -> IO ()) -> IO (SinkTask a)
-- startSink f = do
--   queueA <- newQueue
--   task <- start $ forever $ do
--     a <- popQueue queueA
--     f a

--   pure $
--     SinkTask
--       { task,
--         sink = snd $ toSourceAndSink queueA
--       }

-- -- | Feeds a new value to the sink task
-- writeSinkTask :: (MonadSTM m) => SinkTask a -> a -> m ()
-- writeSinkTask sinkTask = writeSink sinkTask.sink

-- instance AsyncThread (SinkTask a) where
--   type Payload _ = Void
--   await sinkTask = wait sinkTask.task

-- -- | A 'SourceTask' is a task which continously produces values of type 'a'.
-- -- Multiples values can queue up to be consumed with 'readSourceTask'.
-- data SourceTask a = SourceTask
--   { task :: Task Void,
--     source :: Source a
--   }

-- -- | start a 'SourceTask' with the given action. It will be executed forever to produce new values.
-- startSource :: Coordinator -> IO a -> IO (SourceTask a)
-- startSource coordinator f = do
--   queueA <- newQueue
--   task <- start coordinator $ forever $ do
--     a <- f
--     pushQueue queueA a

--   pure $
--     SourceTask
--       { task,
--         source = fst $ toSourceAndSink queueA
--       }

-- -- | Get a value from the 'SourceTask', waiting if there is no output.
-- readSourceTask :: (MonadSTM m) => SourceTask a -> m a
-- readSourceTask sourceTask = readSource sourceTask.source

-- -- | Get a value from the 'SourceTask'. Returns immediately with 'Nothing' if there is no output.
-- tryReadSourceTask :: (MonadSTM m) => SourceTask a -> m (Maybe a)
-- tryReadSourceTask sourceTask = tryReadSource sourceTask.source

-- instance AsyncThread (SourceTask a) where
--   type Payload _ = Void
--   wait sourceTask = wait sourceTask.task
--   cancel sourceTask = cancel sourceTask.task

-- -- | A pipe task transforms values of type 'a' to values of type 'b'.
-- -- Both ends can queue up.
-- data PipeTask a b = PipeTask
--   { task :: Task Void,
--     sink :: Sink a,
--     source :: Source b
--   }

-- -- | start a 'PipeTask' with the given function. It will be executed continously.
-- startPipe :: (a -> IO b) -> ConIO (PipeTask a b)
-- startPipe f = do
--   queueA <- newQueue
--   queueB <- newQueue
--   task <- start $ forever $ do
--     a <- popQueue queueA
--     b <- f a
--     pushQueue queueB b

--   pure $
--     PipeTask
--       { task,
--         sink = snd $ toSourceAndSink queueA,
--         source = fst $ toSourceAndSink queueB
--       }

-- -- | Feeds new values to the pipe task
-- feedPipeTask :: (MonadSTM m) => PipeTask a b -> a -> m ()
-- feedPipeTask pipeTask = writeSink pipeTask.sink

-- -- | Gets the output from the pipe task. Waits if there is output available yet.
-- getPipeTask :: (MonadSTM m) => PipeTask a b -> m b
-- getPipeTask pipeTask = readSource pipeTask.source

-- -- | Gets the output from the pipe task. Does not wait for output.
-- tryGetPipeTask :: (MonadSTM m) => PipeTask a b -> m (Maybe b)
-- tryGetPipeTask pipeTask = tryReadSource pipeTask.source

-- instance AsyncThread (PipeTask a b) where
--   type Payload _ = Void
--   wait pipeTask = wait pipeTask.task
--   cancel pipeTask = cancel pipeTask.task
