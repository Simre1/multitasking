module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef
import Multitasking
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testCase "launch & wait" $ do
          multitask $ \coordinator -> do
            t1 <- start coordinator $ pure (4 :: Int)
            t2 <- start coordinator $ pure (4 :: Int)
            val1 <- await t1
            val2 <- await t2
            liftIO $ val1 @?= val2,
        testCase "launch many tasks" $ multitask $ \coordinator -> do
          counter <- newCounter 0
          tasks <- forM [1 .. 10000] $ \(_i :: Int) -> do
            start coordinator $ incrementCounter counter
          traverse_ await tasks
          value <- getCounter counter
          liftIO $ value @?= 10000,
        testCase "launch really many tasks" $ multitask $ \coordinator -> do
          counter <- newCounter 0
          tasks <- forM [1 .. 100000] $ \(_i :: Int) -> do
            start coordinator $ incrementCounter counter
          traverse_ await tasks
          value <- getCounter counter
          liftIO $ value @?= 100000,
        testCase "wait all" $ do
          ref <- newIORef (0 :: Int)
          multitask $ \coordinator -> do
            _ <- start coordinator $ atomicModifyIORef' ref (\a -> (a + 1, ()))
            _ <- start coordinator $ atomicModifyIORef' ref (\a -> (a + 1, ()))
            _ <- start coordinator $ atomicModifyIORef' ref (\a -> (a + 1, ()))
            awaitAll coordinator
            pure ()
          value <- readIORef ref
          value @?= 3,
        testCase "cancel task" $ do
          ref <- newIORef (0 :: Int)
          gate <- newGate
          multitask $ \coordinator -> do
            t1 <- start coordinator $ atomicModifyIORef' ref (\a -> (a + 1, ()))
            _ <- start coordinator $ awaitGate gate >> atomicModifyIORef' ref (\a -> (a + 1, ()))
            t3 <- start coordinator $ atomicModifyIORef' ref (\a -> (a + 1, ()))
            await t1
            await t3
            pure ()
          openGate gate
          value <- readIORef ref
          value @?= 2,
        testCase "cancel all tasks" $ do
          ref <- newIORef (0 :: Int)
          gate <- newGate
          multitask $ \coordinator -> do
            _ <- start coordinator $ awaitGate gate >> atomicModifyIORef' ref (\a -> (a + 1, ()))
            _ <- start coordinator $ awaitGate gate >> atomicModifyIORef' ref (\a -> (a + 1, ()))
            _ <- start coordinator $ awaitGate gate >> atomicModifyIORef' ref (\a -> (a + 1, ()))
            pure ()
          openGate gate
          value <- readIORef ref
          value @?= 0,
        testCase "wait canceled task 1" $ do
          _ <- assertKilledThread $ do
            t1 <- multitask $ \coordinator -> start coordinator waitForever
            await t1

          pure (),
        testCase "task error propagates to scope" $ do
          assertMultitaskException $ multitask $ \coordinator -> do
            gate <- newGate
            _ <- start coordinator $ undefined >> openGate gate
            _ <- start coordinator $ (pure ())
            awaitGate gate
            pure (),
        testCase "scope exception kills task" $ do
          killedRef <- newIORef False
          assertSomeException $ multitask $ \coordinator -> do
            gate <- newGate
            _ <-
              start coordinator $
                catch @SomeAsyncException
                  (openGate gate >> waitForever)
                  (\e -> writeIORef killedRef True >> throwIO e)
            awaitGate gate
            fail "I die"
          value <- readIORef killedRef
          value @?= True,
        testCase "task exception kills other task" $ do
          killedRef <- newIORef False
          assertMultitaskException $ multitask $ \coordinator -> do
            gate <- newGate
            _ <-
              start coordinator $
                catch @SomeAsyncException
                  (openGate gate >> waitForever)
                  (\e -> writeIORef killedRef True >> throwIO e)
            t <- start coordinator $ awaitGate gate >> fail "I die"
            () <- await t
            pure ()
          value <- readIORef killedRef
          value @?= True,
        testCase "race 2 actions" $ multitask $ \coordinator -> do
          gate1 <- newGate
          gate2 <- newGate
          task :: Task Int <- start coordinator $ raceTwo (awaitGate gate1 >> pure 1) (awaitGate gate2 >> pure 2)
          openGate gate2
          value <- await task
          liftIO $ value @?= 2,
        testCase "race many actions" $ multitask $ \coordinator -> do
          var <- newVariable (-1)
          task <-
            start coordinator $
              raceMany $
                [0 .. 10000] <&> \(i :: Int) -> do
                  awaitCondition (newCondition (== i) (readVariable var)) >> pure i
          writeVariable var 5000
          value <- await task
          liftIO $ value @?= 5000,
        testCase "race 2 tasks" $ multitask $ \coordinator -> do
          gate1 <- newGate
          gate2 <- newGate
          t1 <- start coordinator $ awaitGate gate1 >> pure 1
          t2 <- start coordinator $ awaitGate gate2 >> pure 2
          t3 :: Task Int <- start coordinator $ raceTwo (await t1) (await t2)
          openGate gate2
          value <- await t3
          liftIO $ value @?= 2,
        testCase "race with finished task" $ multitask $ \coordinator -> do
          gate1 <- newGate
          gate2 <- newGate
          t1 <- start coordinator $ awaitGate gate1 >> pure (1 :: Int)
          t2 <- start coordinator $ awaitGate gate2 >> pure (2 :: Int)
          openGate gate2
          _ <- await t2
          t3 <- start coordinator $ raceTwo (await t1) (await t2)
          value <- await t3
          liftIO $ value @?= 2,
        testCase "raceMaybe 2 tasks 1" $ do
          result <- raceTwoMaybe (pure Nothing) (threadDelay 100 >> pure (Just ()))
          liftIO $ result @?= Just (),
        testCase "raceMaybe 2 tasks 2" $ do
          result <- raceTwoMaybe (pure (Just ())) (threadDelay 100 >> pure Nothing)
          liftIO $ result @?= Just (),
        testCase "raceMaybe 2 tasks 3" $ do
          result :: Maybe Int <- raceTwoMaybe (pure Nothing) (threadDelay 100 >> pure Nothing)
          liftIO $ result @?= Nothing,
        testCase "raceMaybe many tasks" $ do
          result :: Maybe Int <- raceManyMaybe [pure Nothing, threadDelay 100 >> pure Nothing]
          liftIO $ result @?= Nothing,
        testCase "raceMaybe many tasks" $ do
          result <- raceManyMaybe [pure Nothing, threadDelay 100 >> pure (Just ())]
          liftIO $ result @?= Just (),
        testCase "raceMaybe many tasks" $ do
          result <- raceManyMaybe [pure (Just ()), threadDelay 10000 >> pure undefined]
          liftIO $ result @?= Just (),
        testCase "timeout" $ do
          result :: Maybe Int <- timeout (fromMilliseconds 10) waitForever
          liftIO $ result @?= Nothing,
        testCase "max concurrency" $ multitask $ \coordinator -> do
          limit <- maxConcurrentTasks 3
          counter <- newCounter 0
          forM_ [0 .. 100] $ \i ->
            start coordinator $
              throttle limit $
                incrementCounter counter *> waitDuration (fromMilliseconds 10)
          waitDuration (fromMilliseconds 15)
          result <- getCounter counter
          liftIO $ result @?= 6,
        testCase "token bucket" $ multitask $ \coordinator -> do
          limit <- tokenBucket coordinator (fromMilliseconds 3) 3
          counter <- newCounter 0
          forM_ [0 .. 100] $ \i ->
            start coordinator $
              throttle limit $
                incrementCounter counter
          waitDuration (fromMilliseconds 13)
          result <- getCounter counter
          liftIO $ result @?= 7
      ]

assertSomeException :: IO a -> IO ()
assertSomeException action = do
  result <- try @SomeException action
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "Expected SomeException, but none thrown"

assertKilledThread :: IO a -> IO ()
assertKilledThread action = do
  result <- try @SomeAsyncException action
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "Expected a killed thread, but not killed"

assertMultitaskException :: IO a -> IO ()
assertMultitaskException action = do
  result <- try @SomeException action
  case result of
    Left _ -> return ()
    Right _ -> assertFailure "Expected an exception, but none thrown"
