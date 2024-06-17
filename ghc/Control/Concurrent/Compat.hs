module Control.Concurrent.Compat
  ( module Control.Concurrent.Compat
  , module Control.Concurrent.STM
  , module Control.Concurrent
  )
import Control.Concurrent.STM
import Control.Concurrent

noRetry :: IO ()
noRetry = return ()

retrying :: IO a -> IO a
retrying = id

-- all the rest are found in ghc
