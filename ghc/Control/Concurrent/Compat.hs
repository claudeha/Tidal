module Control.Concurrent.Compat
  ( module Control.Concurrent.Compat
  , module Control.Concurrent.STM
  , module Control.Concurrent
  ) where
import Control.Concurrent.STM
import Control.Concurrent

noRetry :: Monad m => m ()
noRetry = return ()

retrying :: IO a -> IO a
retrying = id

-- all the rest are found in ghc
