{-# LANGUAGE ScopedTypeVariables #-}

module Web.Postie(
    run
  , runSettingsSocket
  , runSettingsConnection
  , runSettingsConnectionMaker

  , module Web.Postie.Types
  , module Web.Postie.Settings

  , UnexpectedEndOfInputException
  , TooMuchDataException

  , P.Producer
  , P.Consumer
  , P.await
  , P.runEffect
  , (P.>->)
  ) where

import Web.Postie.Settings
import Web.Postie.Connection
import Web.Postie.Types
import Web.Postie.Session
import Web.Postie.Settings
import Web.Postie.Pipes (UnexpectedEndOfInputException, TooMuchDataException)

import Network (PortID (PortNumber), withSocketsDo, listenOn)
import Network.Socket (Socket, SockAddr, accept, sClose)

import System.Timeout

import Control.Monad (forever, void)
import Control.Exception as E
import Control.Concurrent

import qualified Pipes as P

run :: Int -> Application -> IO ()
run port = runSettings (defaultSettings { settingsPort = PortNumber (fromIntegral port) })

runSettings :: Settings -> Application-> IO ()
runSettings settings app = withSocketsDo $
    bracket (listenOn port) sClose $ \socket ->
      runSettingsSocket settings socket app
  where
    port = settingsPort settings

runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket settings socket app = do
    policy <- startTlsPolicy
    runSettingsConnection settings (getConn policy) app
  where
    startTlsPolicy = do
      tlsServerParams <- settingsServerParams settings
      return $ case tlsServerParams of
        (Just params) | settingsDemandSecure settings -> Demand params
                      | settingsAllowSecure settings  -> Allow params
        _                                             -> NotAvailable

    getConn policy = do
      (s, sa) <- accept socket
      conn <- socketConnection s policy
      return (conn, sa)

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection settings getConn = runSettingsConnectionMaker settings getConnMaker
  where
    getConnMaker = do
      (conn, sa) <- getConn
      let mkConn = do
            return conn
      return (mkConn, sa)

runSettingsConnectionMaker :: Settings -> IO (IO Connection, SockAddr) -> Application -> IO ()
runSettingsConnectionMaker settings getConnMaker app = do
    settingsBeforeMainLoop settings
    forever $ do
      (mkConn, sockAddr) <- getConnLoop
      void $ forkIOWithUnmask $ \unmask -> do
          bracket mkConn connClose $ \conn ->
            void $ timeout maxDuration $
              unmask .
              handle onE .
              bracket_ onOpen onClose $
              serveConnection sockAddr settings conn app
      return ()
    return ()
  where
    getConnLoop = getConnMaker `E.catch` \(e :: IOException) -> do
          onE (toException e)
          threadDelay 1000000
          getConnLoop


    onE     = settingsOnException settings
    onOpen  = settingsOnOpen settings
    onClose = settingsOnClose settings

    maxDuration = (settingsTimeout settings) * 1000000

serveConnection :: SockAddr -> Settings -> Connection -> Application -> IO ()
serveConnection _  = runSession
