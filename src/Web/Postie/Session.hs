
module Web.Postie.Session(
    runSession
  , mkSessionEnv
  , mkSessionID
  ) where

import Prelude hiding (lines)

import Web.Postie.Address
import Web.Postie.Types
import Web.Postie.Settings
import Web.Postie.Connection
import Web.Postie.SessionID
import Web.Postie.Protocol (Event(..), Reply, reply, reply', renderReply)
import qualified Web.Postie.Protocol as SMTP
import Web.Postie.Pipes

import qualified Pipes.Parse as P
import qualified Network.TLS as TLS

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

data SessionEnv = SessionEnv {
    sessionID           :: SessionID
  , sessionApp          :: Application
  , sessionSettings     :: Settings
  , sessionConnection   :: Connection
  , sessionServerParams :: Maybe TLS.ServerParams
  }

data SessionState = SessionState {
    sessionProtocol    :: SMTP.SmtpFSM
  , sessionTransaction :: Transaction
  }

type SessionM a = ReaderT SessionEnv (StateT SessionState IO) a

data Transaction = TxnInitial
                 | TxnHaveMailFrom Address
                 | TxnHaveRecipient Address [Address]

mkSessionEnv :: SessionID -> Application -> Settings -> Connection -> Maybe TLS.ServerParams -> SessionEnv
mkSessionEnv = SessionEnv

runSession :: SessionEnv -> IO ()
runSession env = evalStateT (runReaderT startSession env) session
  where
    session = SessionState {
      sessionProtocol    = SMTP.initSmtpFSM
    , sessionTransaction = TxnInitial
    }

startSession :: SessionM ()
startSession = do
  sendReply $ reply 220 "hello!"
  sessionLoop

sessionLoop :: SessionM ()
sessionLoop = do
    (event, fsm') <- SMTP.step <$> getSmtpFsm <*> getCommand <*> getTlsStatus
    case event of
      WantQuit -> do
        sendReply $ reply 221 "goodbye"
        return ()
      _        -> do
        modify (\ss -> ss { sessionProtocol = fsm' })
        handleEvent event >> sessionLoop
  where
    getSmtpFsm   = gets sessionProtocol
    getTlsStatus = do
      SessionEnv {
        sessionConnection = conn
      , sessionSettings   = settings
      } <- ask

      isSecure <- liftIO (connIsSecure conn)

      return $ case settingsStartTLSPolicy settings of
        Just p | isSecure            -> SMTP.Active
               | p == AllowStartTLS  -> SMTP.Permitted
               | p == DemandStartTLS -> SMTP.Required
        _                            -> SMTP.Forbidden

handleEvent :: SMTP.Event -> SessionM ()
handleEvent (SayHelo x)      = do
  SessionEnv {
    sessionID       = sid
  , sessionSettings = settings
  } <- ask

  let handler = settingsOnHello settings

  result  <- liftIO $ handler sid x
  handlerResponse result (sendReply ok)

handleEvent (SayEhlo x)      = do
  SessionEnv {
    sessionID       = sid
  , sessionSettings = settings
  } <- ask

  let handler = settingsOnHello settings

  result  <- liftIO $ handler sid x
  handlerResponse result $
    sendReply =<< ehloAdvertisement

handleEvent (SayEhloAgain _) = sendReply ok
handleEvent (SayHeloAgain _) = sendReply ok
handleEvent SayOK            = sendReply ok

handleEvent (SetMailFrom x)  = do

  SessionEnv {
    sessionID       = sid
  , sessionSettings = settings
  } <- ask

  let handler = settingsOnMailFrom settings

  result  <- liftIO $ handler sid x
  handlerResponse result $ do
    modify (\ss -> ss { sessionTransaction = TxnHaveMailFrom x })
    sendReply ok

handleEvent (AddRcptTo x)   = do

  SessionEnv {
    sessionID        = sid
  , sessionSettings  = settings
  } <- ask

  let handler = settingsOnRecipient settings

  result  <- liftIO $ handler sid x
  handlerResponse result $ do
    txn <- gets sessionTransaction
    let txn' = case txn of
              (TxnHaveMailFrom y)     -> TxnHaveRecipient y [x]
              (TxnHaveRecipient y xs) -> TxnHaveRecipient y (x:xs)
              _                       -> error "impossible"
    modify (\ss -> ss {sessionTransaction = txn' })
    sendReply ok

handleEvent StartData       = do
    sendReply $ reply 354 "End data with <CR><LF>.<CR><LF>"

    SessionEnv {
      sessionID         = sid
    , sessionApp        = app
    , sessionSettings   = settings
    , sessionConnection = conn
    } <- ask

    (TxnHaveRecipient sender recipients) <- gets sessionTransaction
    let chunks = dataChunks (settingsMaxDataSize settings) (toProducer conn)
    let mail   = Mail sid sender recipients chunks

    result <- liftIO $ app mail
    handlerResponse result $ do
      sendReply ok
      modify (\ss -> ss { sessionTransaction = TxnInitial })

handleEvent WantTls = do

  SessionEnv {
      sessionID           = sid
    , sessionConnection   = conn
    , sessionSettings     = settings
    , sessionServerParams = Just serverParams
    } <- ask

  let handler     = settingsOnStartTLS settings

  liftIO $ handler sid
  sendReply ok

  liftIO $ connSetSecure conn serverParams
  modify (\ss -> ss { sessionTransaction = TxnInitial })

handleEvent WantReset = do
  sendReply ok
  modify (\ss -> ss { sessionTransaction = TxnInitial })

handleEvent TlsAlreadyActive =
  sendReply $ reply 454 "STARTTLS not support (already active)"

handleEvent TlsNotSupported =
  sendReply $ reply 454 "STARTTLS not supported"

handleEvent NeedStartTlsFirst =
  sendReply $ reply 530 "Issue STARTTLS first"

handleEvent NeedHeloFirst =
  sendReply $ reply 503 "Need EHLO first"

handleEvent NeedMailFromFirst =
  sendReply $ reply 503 "Need MAIL FROM first"

handleEvent NeedRcptToFirst =
  sendReply $ reply 503 "Need RCPT TO first"

handleEvent _ = error "impossible"

handlerResponse :: HandlerResponse -> SessionM () -> SessionM ()
handlerResponse Accepted action = action
handlerResponse Rejected _      = sendReply reject

getCommand :: SessionM SMTP.Command
getCommand = do
    input   <- toProducer `fmap` asks sessionConnection
    result  <- liftIO $ P.evalStateT (attoParser SMTP.parseCommand) input
    case result of
      Nothing       -> do
        sendReply $ reply 500 "Syntax error, command unrecognized"
        getCommand
      Just command  -> return command

ehloAdvertisement :: SessionM Reply
ehloAdvertisement = do
    stls <- startTls
    let extensions = "8BITMIME" : stls
    return $ reply' 250 (extensions ++ ["OK"])
  where
    startTls = do
      SessionEnv {
        sessionConnection = conn
      , sessionSettings   = settings
      } <- ask
      secure   <- liftIO (connIsSecure conn)
      return ["STARTTLS" | not secure && (
        case settingsStartTLSPolicy settings of
          Just _ -> True
          _ -> False)]

ok :: Reply
ok = reply 250 "OK"

reject :: Reply
reject = reply 554 "Transaction failed"

sendReply :: Reply -> SessionM ()
sendReply r = do
  conn <- asks sessionConnection
  liftIO $ connSend conn (renderReply r)
