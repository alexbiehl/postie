module Network.Mail.Postie.Settings
  ( Settings (..),
    TLSSettings (..),
    StartTLSPolicy (..),
    settingsStartTLSPolicy,
    defaultExceptionHandler,
    mkServerParams,
    def,
    -- | reexport from Default class
  )
where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.Default
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket (HostName, PortNumber, SockAddr)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import System.IO (hPrint, stderr)
import System.IO.Error (ioeGetErrorType)
import Network.Mail.Postie.Address
import Network.Mail.Postie.SessionID
import Network.Mail.Postie.Types
import Prelude

-- | Settings to configure posties behaviour.
data Settings
  = Settings
      { -- | Port postie will run on.
        settingsPort :: PortNumber,
        -- | Timeout for connections in seconds
        settingsTimeout :: Int,
        -- | Maximal size of incoming mail data
        settingsMaxDataSize :: Int,
        -- | Hostname which is shown in posties greeting.
        settingsHost :: Maybe HostName,
        -- | TLS settings if you wish to secure connections.
        settingsTLS :: Maybe TLSSettings,
        -- | Whether authentication is required
        settingsRequireAuth :: Bool,
        -- | Exception handler (default is defaultExceptionHandler)
        settingsOnException :: Maybe SessionID -> SomeException -> IO (),
        -- | Action will be performed before main processing begins.
        settingsBeforeMainLoop :: IO (),
        -- | Action will be performed when connection has been opened.
        settingsOnOpen :: SessionID -> SockAddr -> IO (),
        -- | Action will be performed when connection has been closed.
        settingsOnClose :: SessionID -> IO (),
        -- | Action will be performend on STARTTLS command.
        settingsOnStartTLS :: SessionID -> IO (),
        -- | Performed when client says hello
        settingsOnHello :: SessionID -> ByteString -> IO HandlerResponse,
        -- | Performed when client authenticates
        settingsOnAuth :: SessionID -> ByteString -> IO HandlerResponse,
        -- | Performed when client starts mail transaction
        settingsOnMailFrom :: SessionID -> Address -> IO HandlerResponse,
        -- | Performed when client adds recipient to mail transaction.
        settingsOnRecipient :: SessionID -> Address -> IO HandlerResponse
      }

instance Default Settings where
  def = defaultSettings

-- | Default settings for postie
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsPort = 3001,
      settingsTimeout = 1800,
      settingsMaxDataSize = 32000,
      settingsHost = Nothing,
      settingsTLS = Nothing,
      settingsRequireAuth = False,
      settingsOnException = defaultExceptionHandler,
      settingsBeforeMainLoop = return (),
      settingsOnOpen = \_ _ -> return (),
      settingsOnClose = const $ return (),
      settingsOnStartTLS = const $ return (),
      settingsOnAuth = void,
      settingsOnHello = void,
      settingsOnMailFrom = void,
      settingsOnRecipient = void
    }
  where
    void _ _ = return Accepted

-- | Settings for TLS handling
data TLSSettings
  = TLSSettings
      { -- | Path to certificate file
        certFile :: FilePath,
        -- | Path to private key file belonging to certificate
        keyFile :: FilePath,
        -- | Connection security mode, default is DemandStartTLS
        security :: StartTLSPolicy,
        -- | Logging for TLS
        tlsLogging :: TLS.Logging,
        -- | Supported TLS versions
        tlsAllowedVersions :: [TLS.Version],
        -- | Supported ciphers
        tlsCiphers :: [TLS.Cipher]
      }

instance Default TLSSettings where
  def = defaultTLSSettings

-- | Connection security policy, either via STARTTLS command or on connection initiation.
data StartTLSPolicy
  = -- | Allows clients to use STARTTLS command
    AllowStartTLS
  | -- | Client needs to send STARTTLS command before issuing a mail transaction
    DemandStartTLS
  | -- | Negotiates a TSL context on connection startup.
    ConnectWithTLS
  deriving (Eq, Show)

defaultTLSSettings :: TLSSettings
defaultTLSSettings =
  TLSSettings
    { certFile = "certificate.pem",
      keyFile = "key.pem",
      security = DemandStartTLS,
      tlsLogging = def,
      tlsAllowedVersions = [TLS.SSL3, TLS.TLS10, TLS.TLS11, TLS.TLS12],
      tlsCiphers = TLS.ciphersuite_default
    }

settingsStartTLSPolicy :: Settings -> Maybe StartTLSPolicy
settingsStartTLSPolicy settings = security `fmap` settingsTLS settings

mkServerParams :: TLSSettings -> IO TLS.ServerParams
mkServerParams tlsSettings = do
  credentials <- loadCredentials
  return
    def
      { TLS.serverShared =
          def
            { TLS.sharedCredentials = TLS.Credentials [credentials]
            },
        TLS.serverSupported =
          def
            { TLS.supportedCiphers = tlsCiphers tlsSettings,
              TLS.supportedVersions = tlsAllowedVersions tlsSettings
            }
      }
  where
    loadCredentials =
      either (throw . TLS.Uncontextualized . TLS.Error_Certificate) id
        <$> TLS.credentialLoadX509 (certFile tlsSettings) (keyFile tlsSettings)

defaultExceptionHandler :: Maybe SessionID -> SomeException -> IO ()
defaultExceptionHandler _ e = throwIO e `catches` handlers
  where
    handlers = [Handler ah, Handler oh, Handler tlsh, Handler th, Handler sh]
    ah :: AsyncException -> IO ()
    ah ThreadKilled = return ()
    ah x = hPrint stderr x
    oh :: IOException -> IO ()
    oh x
      | et == ResourceVanished || et == InvalidArgument = return ()
      | otherwise = hPrint stderr x
      where
        et = ioeGetErrorType x
    tlsh :: TLS.TLSException -> IO ()
    tlsh TLS.Terminated {} = return ()
    tlsh TLS.HandshakeFailed {} = return ()
    tlsh x = hPrint stderr x
    th :: TLS.TLSException -> IO ()
    th (TLS.Uncontextualized (TLS.Error_EOF)) = return ()
    th (TLS.Uncontextualized (TLS.Error_Packet_Parsing _)) = return ()
    th (TLS.Uncontextualized (TLS.Error_Packet _)) = return ()
    th (TLS.Uncontextualized (TLS.Error_Protocol _ _)) = return ()
    th x = hPrint stderr x
    sh :: SomeException -> IO ()
    sh = hPrint stderr
