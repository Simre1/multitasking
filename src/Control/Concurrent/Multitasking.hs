module Control.Concurrent.Multitasking where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Ki.Unlifted qualified as Ki
import System.IO.Unsafe

-- newtype Coordinator = Coordinator Ki.Scope

-- newtype Task m = Task (ReaderT Ki.Scope IO a) deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

-- runTask :: Task a -> IO a
-- runTask (Task task) =
--   Ki.scoped
--     ( \scope -> do
--         !x <- runReaderT task scope
--         atomically $ Ki.awaitAll scope
--         pure x
--     )

-- async :: IO a -> Task a
-- async action = Task $ do
--   scope <- ask
--   thread <- liftIO $ Ki.fork scope action
--   liftIO $ unsafeDupableInterleaveIO $ atomically $ Ki.await thread

-- race :: Task a -> Task a -> Task a
-- race t1 t2 = async $ do
--   !x <- liftIO $ Ki.scoped $ \innerScope -> do
--     mvar <- newEmptyMVar
--     _ <- Ki.fork innerScope (runTask t1 >>= tryPutMVar mvar)
--     _ <- Ki.fork innerScope (runTask t2 >>= tryPutMVar mvar)
--     readMVar mvar
--   pure x

-- raceMany :: NonEmpty (Task a) -> Task a
-- raceMany tasks = async $ do
--   !x <- liftIO $ Ki.scoped $ \innerScope -> do
--     mvar <- newEmptyMVar
--     forM_ tasks $ \task ->
--       Ki.fork innerScope (runTask task >>= tryPutMVar mvar)
--     readMVar mvar
--   pure x

-- data Infinite a = Infinite a (Infinite a) deriving (Functor, Foldable, Traversable)

-- data Step a = Value a (MVar (Step a)) | Done

-- asyncProduce :: IO (Maybe a) -> Task [a]
-- asyncProduce action = Task $ do
--   scope <- ask
--   liftIO $ do
--     initial <- newEmptyMVar
--     void $ Ki.fork scope $ do
--       current <- newIORef initial
--       let loop = do
--             result <- action
--             case result of
--               Nothing ->
--                 readIORef current >>= \mvar -> putMVar mvar Done
--               Just value -> do
--                 newMVar <- newEmptyMVar
--                 oldMVar <- atomicModifyIORef' current (newMVar,)
--                 putMVar oldMVar (Value value newMVar)
--                 loop
--       loop

--     let read mvar = unsafeInterleaveIO $ do
--           result <- readMVar mvar
--           case result of
--             Done -> pure []
--             Value a next -> (a :) <$> read next

--     read initial

-- data InfiniteStep a = InfiniteValue a (MVar (InfiniteStep a))

-- asyncForever :: IO a -> Task (Infinite a)
-- asyncForever action = Task $ do
--   scope <- ask
--   liftIO $ do
--     initial <- newEmptyMVar
--     void $ Ki.fork scope $ do
--       current <- newIORef initial
--       let loop = do
--             result <- action
--             newMVar <- newEmptyMVar
--             oldMVar <- atomicModifyIORef' current (newMVar,)
--             putMVar oldMVar (InfiniteValue result newMVar)
--             loop
--       loop

--     let read mvar = unsafeInterleaveIO $ do
--           (InfiniteValue a next) <- readMVar mvar
--           Infinite a <$> read next

--     read initial
