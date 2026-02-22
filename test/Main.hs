module Main (main) where

import Control.Concurrent.Multitasking
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "multitasking tests"
      [ testCase "async & wait" $ runTask $ do
          val1 <- async $ pure (4 :: Int)
          val2 <- async $ pure (4 :: Int)
          liftIO $ val1 @?= val2,
        testCase "async many tasks" $ do
          counter <- newTVarIO (0 :: Int)
          runTask $ do
            forM_ [1 .. 10000] $ \(_i :: Int) ->
              async $ atomically $ modifyTVar' counter (+ 1)
          value <- readTVarIO counter
          liftIO $ value @?= 10000,
        testCase "async really many tasks" $ do
          counter <- newTVarIO (0 :: Int)
          runTask $ do
            forM_ [1 .. 1000000] $ \(_i :: Int) ->
              async $ atomically $ modifyTVar' counter (+ 1)
          value <- readTVarIO counter
          liftIO $ value @?= 1000000,
        testCase "streaming2" $ runTask $ do
          ref <- liftIO $ newIORef 1000
          stream <- asyncProduce $ do
            x <- atomicModifyIORef' ref (\x -> (pred x, x))
            pure $ do
              guard $ x /= 0
              Just x
          liftIO $ stream @?= [1000, 999 .. 1]
      ]
