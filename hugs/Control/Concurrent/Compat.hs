module Control.Concurrent.Compat
  ( module Control.Concurrent.Compat
  , module Control.Concurrent.STM
  , module Control.Concurrent
  )
import Control·Concurrent
import Control.Concurrent.STM

import Control.Monad (unless)

-- fake retry using yielding busyloop for hugs

retry, noRetry :: STM Bool
retry = return False
noRetry = return True

retrying :: IO Bool -> IO ()
retrying m = do
  r <- m
  unless r (yield >> retrying m)

-- found in stm, but not for hugs

swapTVar :: TVar a -> a -> STM a
swapTVar v a = do
  b <- readTVar v
  writeTVar v a
  return b

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x

-- obviously wrong but lets things load in hugs

threadDelay :: Int -> IO ()
threadDelay _ = yield -- FIXME

forkOS :: IO () -> IO ThreadId
forkOS = forkIO -- FIXME
