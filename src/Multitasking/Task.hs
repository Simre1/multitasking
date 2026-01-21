module Multitasking.Task where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import Data.Map qualified as M
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Ki qualified
import System.IO.Unsafe
import System.Mem
import System.Mem.Weak

newtype Task a = Task (ReaderT Ki.Scope IO a) deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runTask :: Task a -> IO a
runTask (Task task) =
  Ki.scoped
    ( \scope -> do
        !x <- runReaderT task scope
        atomically $ Ki.awaitAll scope
        pure x
    )

launch :: IO a -> Task a
launch action = Task $ do
  scope <- ask
  thread <- liftIO $ Ki.fork scope action
  liftIO $ unsafeDupableInterleaveIO $ atomically $ Ki.await thread

race :: Task a -> Task a -> Task a
race t1 t2 = Task $ do
  !x <- liftIO $ Ki.scoped $ \innerScope -> do
    mvar <- newEmptyMVar
    _ <- Ki.fork innerScope (runTask t1 >>= tryPutMVar mvar)
    _ <- Ki.fork innerScope (runTask t2 >>= tryPutMVar mvar)
    readMVar mvar
  pure x

data Infinite a = Infinite a (Infinite a) deriving (Functor, Foldable, Traversable)

launchStream :: IO (Maybe a) -> Task (Stream a)
launchStream action = Task $ do
  scope <- ask
  liftIO $ do
    tvar <- newTVarIO M.empty
    done <- newTVarIO False
    let loop = do
          res <- action
          case res of
            Nothing -> atomically $ writeTVar done True
            Just x -> do
              atomically $ modifyTVar' tvar (\m -> M.insert (M.size m) x m)
              loop
        get i = do
          atomically $ do
            m <- readTVar tvar
            case M.lookup i m of
              Nothing -> do
                d <- readTVar done
                check d
                pure Nothing
              Just x -> pure $ Just x
        read i = unsafeDupableInterleaveIO $ do
          x <- get i
          case x of
            Just v -> do
              vs <- read' (i + 1)
              pure (v : vs)
            Nothing -> pure []
        read' i = do
          addFinalizer (read i) (atomically (modifyTVar' tvar (M.delete i)))
          read i
    _ <- Ki.fork scope loop
    Stream <$> read' 0

newtype Stream a = Stream [a] deriving (Functor, Applicative, Monad, Eq, Ord, Show, Traversable, Foldable)

test :: Task [Int]
test = do
  a <- launch (threadDelay 1000000 >> pure 1)
  b <- launch (pure 2)
  let x = [b, a]
  liftIO $ print x
  pure x

test2 :: Task Int
test2 = race (launch (threadDelay 10 >> pure 1)) (launch (pure 2))

test3 :: Task ()
test3 = do
  ref <- liftIO $ newIORef 3
  stream <-
    launchStream
      ( do
          v <- readIORef ref
          threadDelay 500000
          writeIORef ref (v - 1)
          pure (if v > 0 then Just v else Nothing)
      )
  liftIO (traverse_ print stream)
  pure ()

test4 :: Task ()
test4 = do
  a <- launch $ threadDelay 500000 >> putStrLn "A"
  liftIO $ putStrLn "M"
  case a of
    () -> liftIO $ putStrLn "E"
  pure ()
