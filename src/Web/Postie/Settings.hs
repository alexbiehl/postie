
module Web.Postie.Settings(
    Settings(..)
  , defaultSettings
  , TLSSettings(..)
  , tlsSettings
  , defaultTLSSettings
  , defaultExceptionHandler
  ) where

import Web.Postie.Types

import Network (HostName, PortID(..), Socket)
import Control.Exception
import GHC.IO.Exception (IOErrorType(..))
import System.IO (hPrint, stderr)
import System.IO.Error (ioeGetErrorType)

import qualified Data.Text as T

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Data.Default.Class

-- | Settings to configure posties behaviour.
data Settings = Settings {
    settingsPort            :: PortID -- ^ Port postie will run on.
  , settingsTimeout         :: Int    -- ^ Timeout for connections.
  , settingsHost            :: Maybe HostName -- ^ Hostname which is shown in posties greeting.
  , settingsTLS             :: Maybe TLSSettings -- ^ TLS settings if you wish to secure connections.
  , settingsOnException     :: SomeException -> IO () -- ^ Exception handler (default is defaultExceptionHandler)
  , settingsOnOpen          :: IO () -- ^ Action will be performed when connection has been opened.
  , settingsOnClose         :: IO () -- ^ Action will be performed when connection has been closed.
  , settingsBeforeMainLoop  :: IO () -- ^ Action will be performed before main processing begins.
  , settingsOnMailFrom      :: T.Text -> IO HandlerResponse
  , settingsOnRecipient     :: T.Text -> IO HandlerResponse
  }

defaultSettings :: Settings
defaultSettings = Settings {
    settingsPort             = PortNumber 3001
  , settingsTimeout         = 1800
  , settingsHost            = Nothing
  , settingsTLS             = Nothing
  , settingsOnException     = defaultExceptionHandler
  , settingsOnOpen          = return ()
  , settingsOnClose         = return ()
  , settingsBeforeMainLoop  = return ()
  , settingsOnMailFrom      = const $ return Accepted
  , settingsOnRecipient     = const $ return Accepted
  }

data TLSSettings = TLSSettings {
    certFile           :: FilePath
  , keyFile            :: FilePath
  , security           :: ConnectionSecurity
  , tlsLogging         :: TLS.Logging
  , tlsAllowedVersions :: [TLS.Version]
  , tlsCiphers         :: [TLS.Cipher]
  }

data ConnectionSecurity = AllowSecure
                        | DemandSecure

defaultTLSSettings :: TLSSettings
defaultTLSSettings = TLSSettings {
    certFile           = "certificate.pem"
  , keyFile            = "key.pem"
  , security           = DemandSecure
  , tlsLogging         = def
  , tlsAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
  , tlsCiphers         = TLS.ciphersuite_all
  }

tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings cert key = defaultTLSSettings {
    certFile = cert
  , keyFile  = key
  }

defaultExceptionHandler :: SomeException -> IO ()
defaultExceptionHandler e = throwIO e `catches` handlers
  where
    handlers = [Handler ah, Handler oh, Handler sh]

    ah :: AsyncException -> IO ()
    ah ThreadKilled = return ()
    ah x            = hPrint stderr x

    oh :: IOException -> IO ()
    oh x
      | et == ResourceVanished || et == InvalidArgument = return ()
      | otherwise         = hPrint stderr x
      where
        et = ioeGetErrorType x

    sh :: SomeException -> IO ()
    sh x = hPrint stderr x
