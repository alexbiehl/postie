{-# LANGUAGE ScopedTypeVariables #-}

module Web.Postie(
    run
  , runSettingsSocket
  , runSettingsConnection
  , runSettingsConnectionMaker
  ) where

import Web.Postie.Settings
import Web.Postie.Connection
import Web.Postie.Types

import Network (PortID (PortNumber), withSocketsDo, listenOn)
import Network.Socket (Socket, SockAddr, accept, sClose)

import Control.Monad
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
runSettingsSocket settings socket = runSettingsConnection settings getConn
  where
    getConn = do
      (s, sa) <- accept socket
      conn <- socketConnection s
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
          unmask .
          handle onE .
          bracket_ onOpen onClose
          $ serveConnection conn sockAddr settings app
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

serveConnection :: Connection -> SockAddr -> Settings -> Application -> IO ()
serveConnection conn _ settings app = undefined
