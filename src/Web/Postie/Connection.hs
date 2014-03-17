module Web.Postie.Connection(
    Connection,
    StartTLSPolicy(..),

    connRecv,
    connSend,
    connClose,
    connIsSecure,

    connStartTls,
    connAllowStartTLS,
    connDemandStartTLS,

    socketConnection,

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
    connRecv           :: IO BS.ByteString -- ^ Reads data from connection. Returns empty bytestring if eof is reached.
  , connSend           :: LBS.ByteString -> IO () -- ^ Sends data over the connection.
  , connClose          :: IO ()    -- ^Closes the connection.
  , connIsSecure       :: Bool     -- ^Returns true if this is a TLS-secured connection.
  , connStartTlsPolicy :: StartTLSPolicy
  , connStartTls       :: IO Connection -- ^Creates new connection which is secured by TLS.
  }

data StartTLSPolicy = Always ServerParams | Allow ServerParams | Demand ServerParams | NotAvailable

-- | Upgradeable connection from Socket
socketConnection :: Socket -> StartTLSPolicy -> IO Connection
socketConnection socket policy     = case policy of
                                      (Always _) -> secureConnection
                                      _          -> return connection
  where
    connection = Connection {
      connRecv     = recv socket defaultChunkSize
    , connSend     = sendAll socket
    , connClose    = sClose socket
    , connIsSecure = False
    , connStartTlsPolicy = policy
    , connStartTls = secureConnection
    }

    secureConnection = do
      context <- contextNew socket params =<< makeSystem
      handshake context

      return Connection {
          connRecv     = recvData context
        , connSend     = sendData context
        , connClose    = bye context `finally` contextClose context
        , connIsSecure = True
        , connStartTlsPolicy = policy
        , connStartTls = error "already on secure connection"
      }

    params = case policy of
      (Allow p)  -> p
      (Demand p) -> p
      (Always p) -> p
      _          -> error "no upgrade allowed"

connAllowStartTLS :: Connection -> Bool
connAllowStartTLS conn | connIsSecure conn = False
                       | allowedByPolicy (connStartTlsPolicy conn) = True
                       | otherwise         = False
  where
    allowedByPolicy (Allow _)   = True
    allowedByPolicy (Demand _)  = True
    allowedByPolicy _           = False

connDemandStartTLS :: Connection -> Bool
connDemandStartTLS conn | connIsSecure conn = False
                        | demandByPolicy (connStartTlsPolicy conn) = True
                        | otherwise         = False
  where
    demandByPolicy (Demand _) = True
    demandByPolicy _          = False

connectionP :: (MonadIO m) => Connection -> P.Producer' BS.ByteString m ()
connectionP conn = go
  where go = do
          bs <- liftIO $ connRecv conn
          if BS.null bs then
            return ()
            else
              P.yield bs >> go
