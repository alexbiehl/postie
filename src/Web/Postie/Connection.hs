module Web.Postie.Connection(
    Connection,

    connRecv,
    connSend,
    connClose,
    connIsSecure,
    connStartTls,

    socketConnection,
    secureSocketConnection,

    connectionP
  ) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (sendAll)
import Network.Socket.ByteString hiding (sendAll)

import Network.TLS
import Crypto.Random.AESCtr

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import Control.Exception (finally)
import Control.Monad.IO.Class

import qualified Pipes as P

-- |Low-level connection abstraction
data Connection = Connection {
    connRecv      :: IO BS.ByteString -- ^ Reads data from connection. Returns empty bytestring if eof is reached.
  , connSend      :: LBS.ByteString -> IO () -- ^ Sends data over the connection.
  , connClose     :: IO ()    -- ^Closes the connection.
  , connIsSecure  :: Bool     -- ^Returns true if this is a TLS-secured connection.
  , connStartTls  :: ServerParams -> IO Connection -- ^Creates new connection which is secured by TLS.
  }

-- | Insecure connection from socket.
socketConnection :: Socket -> IO Connection
socketConnection socket = return connection
  where
    connection = Connection {
      connRecv     = recv socket defaultChunkSize
    , connSend     = sendAll socket
    , connClose    = sClose socket
    , connIsSecure = False
    , connStartTls = secureSocketConnection socket
    }

-- | Creates a secured connection from a socket and given Serverparams
secureSocketConnection :: Socket -> ServerParams -> IO Connection
secureSocketConnection socket params = do
  context <- contextNew socket params =<< makeSystem
  handshake context
  return Connection {
    connRecv     = recvData context
  , connSend     = sendData context
  , connClose    = bye context `finally` contextClose context
  , connIsSecure = True
  , connStartTls = error "already on secure connection"
  }

connectionP :: (MonadIO m) => Connection -> P.Producer' BS.ByteString m ()
connectionP conn = go
  where go = do
          bs <- liftIO $ connRecv conn
          if BS.null bs then
            return ()
            else
              P.yield bs >> go
