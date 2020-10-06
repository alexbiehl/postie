{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mail.Postie
  ( run,
    -- | Runs server with a given application on a specified port
    runSettings,
    -- | Runs server with a given application and settings
    runSettingsSocket,

    -- * Application
    module Network.Mail.Postie.Types,

    -- * Settings
    module Network.Mail.Postie.Settings,

    -- * Address
    module Network.Mail.Postie.Address,

    -- * Exceptions
    UnexpectedEndOfInputException,
    TooMuchDataException,

    -- * Re-exports
    P.Producer,
    P.Consumer,
    P.runEffect,
    (P.>->),
  )
where

import Control.Concurrent
import Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket
import Network.TLS (ServerParams)
import qualified Pipes as P
import System.Timeout
import Network.Mail.Postie.Address
import Network.Mail.Postie.Connection
import Network.Mail.Postie.Pipes (TooMuchDataException, UnexpectedEndOfInputException)
import Network.Mail.Postie.Session
import Network.Mail.Postie.Settings
import Network.Mail.Postie.Types

run :: Int -> Application -> IO ()
run port = runSettings (def {settingsPort = fromIntegral port})

runSettings :: Settings -> Application -> IO ()
runSettings settings app = withSocketsDo
  $ bracket (listenOn port) close
  $ \sock ->
    runSettingsSocket settings sock app
  where
    port = settingsPort settings
    listenOn portNum =
      bracketOnError
        (socket AF_INET6 Stream defaultProtocol)
        close
        ( \sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrInet6 portNum 0 (0, 0, 0, 0) 0)
            listen sock maxListenQueue
            return sock
        )

runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket settings sock =
  runSettingsConnection settings getConn
  where
    getConn = do
      (s, sa) <- accept sock
      conn <- mkSocketConnection s
      return (conn, sa)

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection settings getConn app = do
  serverParams <- mkServerParams'
  runSettingsConnectionMaker settings (getConnMaker serverParams) serverParams app
  where
    getConnMaker serverParams = do
      (conn, sa) <- getConn
      let mkConn = do
            case settingsStartTLSPolicy settings of
              Just ConnectWithTLS -> do
                let (Just sp) = serverParams
                connSetSecure conn sp
              _ -> return ()
            return conn
      return (mkConn, sa)
    mkServerParams' =
      case settingsTLS settings of
        Just tls -> do
          serverParams <- mkServerParams tls
          return (Just serverParams)
        _ -> return Nothing

runSettingsConnectionMaker ::
  Settings ->
  IO (IO Connection, SockAddr) ->
  Maybe ServerParams ->
  Application ->
  IO ()
runSettingsConnectionMaker settings getConnMaker serverParams app = do
  settingsBeforeMainLoop settings
  void $ forever $ do
    (mkConn, sockAddr) <- getConnLoop
    void $ forkIOWithUnmask $ \unmask -> do
      sessionID <- mkSessionID
      bracket mkConn connClose $ \conn ->
        void $ timeout maxDuration
          $ unmask
            . handle (onE $ Just sessionID)
            . bracket_ (onOpen sessionID sockAddr) (onClose sessionID)
          $ runSession (mkSessionEnv sessionID app settings conn serverParams)
  where
    getConnLoop = getConnMaker `E.catch` \(e :: IOException) -> do
      onE Nothing (toException e)
      threadDelay 1000000
      getConnLoop
    onE = settingsOnException settings
    onOpen = settingsOnOpen settings
    onClose = settingsOnClose settings
    maxDuration = settingsTimeout settings * 1000000
