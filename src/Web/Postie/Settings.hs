
module Web.Postie.Settings(
    Settings(..)
  , TLSSettings(..)
  , StartTLSPolicy(..)
  , settingsStartTLSPolicy
  , defaultExceptionHandler
  , mkServerParams
  , def -- |reexport from Default class
  ) where

import Web.Postie.Types
import Web.Postie.Address
import Web.Postie.SessionID

import Network (HostName, PortID(..))
import System.IO (hPrint, stderr)
import System.IO.Error (ioeGetErrorType)
import Data.ByteString (ByteString)

import Network.Socket (SockAddr)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Data.Default.Class

import Control.Exception
import GHC.IO.Exception (IOErrorType(..))
import Control.Applicative

import Prelude

-- | Settings to configure posties behaviour.
data Settings = Settings {
    settingsPort            :: PortID -- ^ Port postie will run on.
  , settingsTimeout         :: Int    -- ^ Timeout for connections in seconds
  , settingsMaxDataSize     :: Int    -- ^ Maximal size of incoming mail data
  , settingsHost            :: Maybe HostName -- ^ Hostname which is shown in posties greeting.
  , settingsTLS             :: Maybe TLSSettings -- ^ TLS settings if you wish to secure connections.
  , settingsOnException     :: Maybe SessionID -> SomeException -> IO () -- ^ Exception handler (default is defaultExceptionHandler)
  , settingsBeforeMainLoop  :: IO () -- ^ Action will be performed before main processing begins.
  , settingsOnOpen          :: SessionID -> SockAddr -> IO () -- ^ Action will be performed when connection has been opened.
  , settingsOnClose         :: SessionID -> IO () -- ^ Action will be performed when connection has been closed.
  , settingsOnStartTLS      :: SessionID -> IO () -- ^ Action will be performend on STARTTLS command.
  , settingsOnHello         :: SessionID -> ByteString -> IO HandlerResponse -- ^ Performed when client says hello
  , settingsOnMailFrom      :: SessionID -> Address -> IO HandlerResponse -- ^ Performed when client starts mail transaction
  , settingsOnRecipient     :: SessionID -> Address -> IO HandlerResponse -- ^ Performed when client adds recipient to mail transaction.
  }

instance Default Settings where
  def = defaultSettings

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
    , settingsOnOpen          = \_ _ -> return ()
    , settingsOnClose         = const $ return ()
    , settingsOnStartTLS      = const $ return ()
    , settingsOnHello         = void
    , settingsOnMailFrom      = void
    , settingsOnRecipient     = void
    }
  where
    void _ _ = return Accepted


-- | Settings for TLS handling
data TLSSettings = TLSSettings {
    certFile           :: FilePath -- ^ Path to certificate file
  , keyFile            :: FilePath  -- ^ Path to private key file belonging to certificate
  , security           :: StartTLSPolicy -- ^ Connection security mode, default is DemandStartTLS
  , tlsLogging         :: TLS.Logging -- ^ Logging for TLS
  , tlsAllowedVersions :: [TLS.Version] -- ^ Supported TLS versions
  , tlsCiphers         :: [TLS.Cipher] -- ^ Supported ciphers
  }

instance Default TLSSettings where
  def = defaultTLSSettings

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

settingsStartTLSPolicy :: Settings -> Maybe StartTLSPolicy
settingsStartTLSPolicy settings = security `fmap` settingsTLS settings

mkServerParams :: TLSSettings -> IO TLS.ServerParams
mkServerParams tlsSettings = do
    credentials  <- loadCredentials
    return def {
      TLS.serverShared = def {
        TLS.sharedCredentials = TLS.Credentials [credentials]
      },
      TLS.serverSupported = def {
        TLS.supportedCiphers  = tlsCiphers tlsSettings
      , TLS.supportedVersions = tlsAllowedVersions tlsSettings
      }
    }
  where
    loadCredentials = either (throw . TLS.Error_Certificate) id <$>
        TLS.credentialLoadX509 (certFile tlsSettings) (keyFile tlsSettings)

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
    tlsh TLS.Terminated{}      = return ()
    tlsh TLS.HandshakeFailed{} = return ()
    tlsh x                     = hPrint stderr x

    th :: TLS.TLSError -> IO ()
    th TLS.Error_EOF                = return ()
    th (TLS.Error_Packet_Parsing _) = return ()
    th (TLS.Error_Packet _)         = return ()
    th (TLS.Error_Protocol _)       = return ()
    th x                            = hPrint stderr x

    sh :: SomeException -> IO ()
    sh = hPrint stderr
