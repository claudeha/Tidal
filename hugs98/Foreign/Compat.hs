module Foreign.Compat
  ( module Foreign.Compat
  , module Foreign.C.Types
  ) where
import Foreign.C.Types

newtype CBool = CBool CInt
