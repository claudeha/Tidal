module Network.Compat
  ( module Network.Compat
  , module Network.Socket
  ) where

import Network.Socket
getAddrInfo = undefined
type AddrInfo = ()
addrAddress = undefined
defaultHints :: Hints
defaultHints = undefined
data Hints = Hints{ addrSocketType :: SocketType }
