module Sound.Osc.Fd where

type Time = Double

data Udp = Udp

time :: Monad m => m Time
time = return 0 -- FIXME

data Message = Message String [Datum]
  deriving (Eq, Show)

data Datum = Blob () | Int32 Int | Float Double | AsciiString String
  deriving (Eq, Show)

int32 = Int32
float = undefined
string = undefined
blob_pack = undefined
datum_integral = undefined
datum_floating = undefined

data Bundle = Bundle Time [Message]
  deriving (Eq, Show)

data Packet_Bundle = Packet_Bundle Bundle
  deriving (Eq, Show)

data Packet_Message = Packet_Message Message
  deriving (Eq, Show)

packetMessages = undefined

udp_socket = undefined
ntpr_to_posix = undefined
sendTo = undefined
sendBundle = undefined
sendMessage = undefined

class Transport t where
  recvPacketTimeout :: Double -> t -> IO (Maybe Packet_Message)

instance Transport Udp where
  recvPacketTimeout = undefined

udpServer = undefined
udpSocket = undefined
ascii = undefined
ascii_to_string = undefined
