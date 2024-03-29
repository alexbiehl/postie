module Network.Mail.Postie.Connection
  ( Connection,
    connIsSecure,
    connSetSecure,
    connRecv,
    connSend,
    connClose,
    mkSocketConnection,
    toProducer,
  )
where

import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.IORef
import Network.Socket
import Network.Socket.ByteString hiding (sendAll)
import Network.Socket.ByteString.Lazy (sendAll)
import Network.TLS
import qualified Pipes as P

data ConnectionBackend
  = ConnPlain Socket
  | ConnSecure Context

newtype Connection = Connection (IORef ConnectionBackend)

connSetSecure :: Connection -> ServerParams -> IO ()
connSetSecure (Connection cbe) params = do
  backend <- readIORef cbe
  securedBackend <- upgrade backend
  writeIORef cbe securedBackend
  where
    upgrade (ConnPlain be) = do
      context <- contextNew be params
      handshake context
      return (ConnSecure context)
    upgrade (ConnSecure _) = error "already on secure connection"

connIsSecure :: Connection -> IO Bool
connIsSecure (Connection cbe) = do
  backend <- readIORef cbe
  return $ case backend of
    (ConnSecure _) -> True
    _ -> False

mkSocketConnection :: Socket -> IO Connection
mkSocketConnection s = do
  conn <- newIORef (ConnPlain s)
  return (Connection conn)

connBackendRecv :: ConnectionBackend -> IO BS.ByteString
connBackendRecv (ConnPlain s) = recv s defaultChunkSize
connBackendRecv (ConnSecure ctx) = recvData ctx

connBackendSend :: ConnectionBackend -> LBS.ByteString -> IO ()
connBackendSend (ConnPlain s) = sendAll s
connBackendSend (ConnSecure ctx) = sendData ctx

connRecv :: Connection -> IO BS.ByteString
connRecv (Connection cbe) = readIORef cbe >>= connBackendRecv

connSend :: Connection -> LBS.ByteString -> IO ()
connSend (Connection cbe) lbs = do
  backend <- readIORef cbe
  connBackendSend backend lbs

connClose :: Connection -> IO ()
connClose (Connection cbe) = closeBackend =<< readIORef cbe
  where
    closeBackend (ConnPlain s) = close s
    closeBackend (ConnSecure context) = bye context `finally` contextClose context

toProducer :: (MonadIO m) => Connection -> P.Producer BS.ByteString m ()
toProducer conn = go
  where
    go = do
      bs <- liftIO $ connRecv conn
      unless (BS.null bs) $
        P.yield bs >> go
