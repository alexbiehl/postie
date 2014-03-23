
module Web.Postie.Settings(
    Settings(..)
  , defaultSettings
  , TLSSettings(..)
  , StartTLSPolicy(..)
  , tlsSettings
  , defaultTLSSettings
  , defaultExceptionHandler
  , settingsStartTLSPolicy
  , settingsConnectWithTLS
  , settingsAllowStartTLS
  , settingsDemandStartTLS
  ) where

import Web.Postie.Types
import Web.Postie.Address
import qualified Web.Postie.Connection as Connection

import Network (HostName, PortID(..))
import System.IO (hPrint, stderr)
import System.IO.Error (ioeGetErrorType)
import Data.ByteString (ByteString)

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Data.Default.Class
import Data.Maybe (fromMaybe)

import Control.Exception
import GHC.IO.Exception (IOErrorType(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Applicative ((<$>))

-- | Settings to configure posties behaviour.
data Settings = Settings {
    settingsPort            :: PortID -- ^ Port postie will run on.
  , settingsTimeout         :: Int    -- ^ Timeout for connections in seconds
  , settingsMaxDataSize     :: Int    -- ^ Maximal size of incoming mail data
  , settingsHost            :: Maybe HostName -- ^ Hostname which is shown in posties greeting.
  , settingsTLS             :: Maybe TLSSettings -- ^ TLS settings if you wish to secure connections.
  , settingsOnException     :: Maybe SessionID -> SomeException -> IO () -- ^ Exception handler (default is defaultExceptionHandler)
  , settingsBeforeMainLoop  :: IO () -- ^ Action will be performed before main processing begins.
  , settingsOnOpen          :: SessionID -> IO () -- ^ Action will be performed when connection has been opened.
  , settingsOnClose         :: SessionID -> IO () -- ^ Action will be performed when connection has been closed.
  , settingsOnStartTLS      :: SessionID -> IO () -- ^ Action will be performend on STARTTLS command.
  , settingsOnHello         :: SessionID -> ByteString -> IO HandlerResponse -- ^ Performed when client says hello
  , settingsOnMailFrom      :: SessionID -> Address -> IO HandlerResponse -- ^ Performed when client starts mail transaction
  , settingsOnRecipient     :: SessionID -> Address -> IO HandlerResponse -- ^ Performed when client adds recipient to mail transaction.
  }

-- | Default settings for postie
defaultSettings :: Settings
defaultSettings = Settings {
      settingsPort            = PortNumber 3001
    , settingsTimeout         = 1800
    , settingsMaxDataSize     = 32000
    , settingsHost            = Nothing
    , settingsTLS             = Nothing
    , settingsOnException     = defaultExceptionHandler
    , settingsBeforeMainLoop  = return ()
    , settingsOnOpen          = const $ return ()
    , settingsOnClose         = const $ return ()
    , settingsOnStartTLS      = const $ return ()
    , settingsOnHello         = void
    , settingsOnMailFrom      = void
    , settingsOnRecipient     = void
    }
  where
    void = \_ _ -> return Accepted


-- | Settings for TLS handling
data TLSSettings = TLSSettings {
    certFile           :: FilePath -- ^ Path to certificate file
  , keyFile            :: FilePath  -- ^ Path to private key file belonging to certificate
  , security           :: StartTLSPolicy -- ^ Connection security mode, default is DemandStartTLS
  , tlsLogging         :: TLS.Logging -- ^ Logging for TLS
  , tlsAllowedVersions :: [TLS.Version] -- ^ Supported TLS versions
  , tlsCiphers         :: [TLS.Cipher] -- ^ Supported ciphers
  }

-- | Connection security policy, either via STARTTLS command or on connection initiation.
data StartTLSPolicy = AllowStartTLS -- ^ Allows clients to use STARTTLS command
                    | DemandStartTLS -- ^ Client needs to send STARTTLS command before issuing a mail transaction
                    | ConnectWithTLS -- ^ Negotiates a TSL context on connection startup.
                    deriving (Eq, Show)

defaultTLSSettings :: TLSSettings
defaultTLSSettings = TLSSettings {
    certFile           = "certificate.pem"
  , keyFile            = "key.pem"
  , security           = DemandStartTLS
  , tlsLogging         = def
  , tlsAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
  , tlsCiphers         = TLS.ciphersuite_all
  }

-- | Convenience function for creation of TLSSettings taking certificate and key file paths as parameters.
tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings cert key = defaultTLSSettings {
    certFile = cert
  , keyFile  = key
  }

settingsConnectWithTLS :: Settings -> Bool
settingsConnectWithTLS = checkSecurity ConnectWithTLS

settingsAllowStartTLS :: Settings -> Bool
settingsAllowStartTLS = checkSecurity AllowStartTLS

settingsDemandStartTLS :: Settings -> Bool
settingsDemandStartTLS = checkSecurity DemandStartTLS

checkSecurity :: StartTLSPolicy -> Settings -> Bool
checkSecurity p s = fromMaybe False $ do
  tlss <- settingsTLS s
  return (security tlss == p)

settingsStartTLSPolicy :: Settings -> IO Connection.StartTLSPolicy
settingsStartTLSPolicy settings = do
  mserverParams <- settingsServerParams settings
  return $ case mserverParams of
    (Just params) | settingsDemandStartTLS settings -> Connection.Demand params
                  | settingsAllowStartTLS settings  -> Connection.Allow params
                  | settingsConnectWithTLS settings -> Connection.Always params
    _                                               -> Connection.NotAvailable

settingsServerParams :: Settings -> IO (Maybe TLS.ServerParams)
settingsServerParams settings = runMaybeT $ do
    tlss        <- MaybeT . return $ settingsTLS settings
    credential  <- lift $ loadCredentials tlss
    return def {
      TLS.serverShared = def {
        TLS.sharedCredentials = TLS.Credentials [credential]
      },
      TLS.serverSupported = def {
        TLS.supportedCiphers  = (tlsCiphers tlss)
      , TLS.supportedVersions = (tlsAllowedVersions tlss)
      }
    }
  where
    loadCredentials tlss = either (throw . TLS.Error_Certificate) id <$>
        TLS.credentialLoadX509 (certFile tlss) (keyFile tlss)

defaultExceptionHandler :: Maybe SessionID -> SomeException -> IO ()
defaultExceptionHandler _ e = throwIO e `catches` handlers
  where
    handlers = [Handler ah, Handler oh, Handler tlsh, Handler th, Handler sh]

    ah :: AsyncException -> IO ()
    ah ThreadKilled = return ()
    ah x            = hPrint stderr x

    oh :: IOException -> IO ()
    oh x
      | et == ResourceVanished || et == InvalidArgument = return ()
      | otherwise         = hPrint stderr x
      where
        et = ioeGetErrorType x

    tlsh :: TLS.TLSException -> IO ()
    tlsh (TLS.Terminated _ _ _)     = return ()
    tlsh (TLS.HandshakeFailed _)    = return ()
    tlsh x                          = hPrint stderr x

    th :: TLS.TLSError -> IO ()
    th TLS.Error_EOF                = return ()
    th (TLS.Error_Packet_Parsing _) = return ()
    th (TLS.Error_Packet _)         = return ()
    th (TLS.Error_Protocol _)       = return ()
    th x                            = hPrint stderr x

    sh :: SomeException -> IO ()
    sh x = hPrint stderr x
