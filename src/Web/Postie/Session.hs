
module Web.Postie.Session(
    runSession
  ) where

import Prelude hiding (lines)

import Web.Postie.Address
import Web.Postie.Types
import Web.Postie.Settings
import Web.Postie.Connection
import Web.Postie.Protocol (Event(..), Reply, reply, reply', renderReply)
import qualified Web.Postie.Protocol as SMTP
import Web.Postie.Pipes

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

import Pipes (await, yield, lift, (>->))
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as P

import Control.Applicative
import Control.Monad.State

data SessionState = SessionState {
    sessionApp              :: Application
  , sessionSettings         :: Settings
  , sessionConnection       :: Connection
  , sessionConnectionInput  :: P.Producer BS.ByteString IO ()
  , sessionTLSStatus        :: SMTP.TlsStatus
  , sessionProtocolState    :: SMTP.SmtpFSM
  , sessionTransaction      :: Transaction
  }

data Transaction = TxnInitial
                 | TxnHaveMailFrom Address
                 | TxnHaveRecipient Address [Address]

runSession :: Settings -> Connection -> Application -> IO ()
runSession settings connection app =
  evalStateT session (initialSessionState settings connection app)

initialSessionState :: Settings -> Connection -> Application -> SessionState
initialSessionState settings connection app = SessionState {
    sessionApp             = app
  , sessionSettings        = settings
  , sessionConnection      = connection
  , sessionConnectionInput = connectionP connection
  , sessionTLSStatus       = SMTP.Forbidden
  , sessionProtocolState   = SMTP.initSmtpFSM
  , sessionTransaction     = TxnInitial
  }

session :: StateT SessionState IO ()
session = do
    (event, fsm') <- SMTP.step <$> getSmtpFsm <*> getCommand <*> getTlsStatus
    case event of
      WantQuit -> do
        sendReply $ reply 221 "goodbye"
        return ()
      _        -> do
        modify (\ss -> ss { sessionProtocolState = fsm' })
        handleEvent event >> session
  where
    getSmtpFsm   = gets sessionProtocolState
    getTlsStatus = gets sessionTLSStatus

handleEvent :: SMTP.Event -> StateT SessionState IO ()
handleEvent (SayHelo x)      = do
  handler <- settingsOnHello <$> gets sessionSettings
  result  <- liftIO $ handler x
  case result of
    Accepted -> sendReply ok
    _        -> sendReply reject

handleEvent (SayEhlo x)      = do
  handler <- settingsOnHello <$> gets sessionSettings
  result  <- liftIO $ handler x
  case result of
    Accepted -> sendReply =<< ehloAdvertisement
    _        -> sendReply reject

handleEvent (SayEhloAgain _) = sendReply ok
handleEvent (SayHeloAgain _) = sendReply ok
handleEvent SayOK            = sendReply ok

handleEvent (SetMailFrom x)  = do
  handler <- settingsOnMailFrom <$> gets sessionSettings
  result  <- liftIO $ handler x
  case result of
    Accepted -> do
      modify (\ss -> ss { sessionTransaction = TxnHaveMailFrom x })
      sendReply ok
    _        -> sendReply reject

handleEvent (AddRcptTo x)   = do
  handler <- settingsOnRecipient <$> gets sessionSettings
  result  <- liftIO $ handler x
  case result of
    Accepted -> do
                txn <- gets sessionTransaction
                let txn' = case txn of
                          (TxnHaveMailFrom y)     -> TxnHaveRecipient y [x]
                          (TxnHaveRecipient y xs) -> TxnHaveRecipient y (x:xs)
                modify (\ss -> ss {sessionTransaction = txn' })
                sendReply ok
    _        -> sendReply reject

handleEvent StartData       = do
    sendReply $ reply 354 "End data with <CR><LF>.<CR><LF>"
    (TxnHaveRecipient sender recipients) <- gets sessionTransaction
    mail   <- Mail sender recipients <$> chunks
    app    <- gets sessionApp
    result <- liftIO $ app mail
    case result of
      Accepted -> sendReply ok
      Rejected -> sendReply reject
  where
    maxDataLength     = settingsMaxDataSize `fmap` gets sessionSettings
    sessionConnection = gets sessionConnectionInput
    chunks            = dataChunks <$> maxDataLength <*> sessionConnection

handleEvent WantTls = do
  handler <- settingsOnStartTLS <$> gets sessionSettings
  liftIO $ handler
  sendReply reject

handleEvent WantReset = do
  sendReply ok
  modify (\ss -> ss { sessionTransaction = TxnInitial })

handleEvent TlsAlreadyActive = do
  sendReply $ reply 454 "STARTTLS not support (already active)"

handleEvent TlsNotSupported = do
  sendReply $ reply 454 "STARTTLS not supported"

handleEvent NeedStartTlsFirst = do
  sendReply $ reply 530 "Issue STARTTLS first"

handleEvent NeedHeloFirst = do
  sendReply $ reply 503 "Need EHLO first"

handleEvent NeedMailFromFirst = do
  sendReply $ reply 503 "Need MAIL FROM first"

handleEvent NeedRcptToFirst = do
  sendReply $ reply 503 "Need RCPT TO first"

getCommand :: StateT SessionState IO SMTP.Command
getCommand = do
  input   <- gets sessionConnectionInput
  result  <- liftIO $ P.evalStateT (attoParser SMTP.parseCommand) input
  case result of
    Nothing       -> do
      sendReply $ reply 500 "Syntax error, command unrecognized"
      getCommand
    Just command  -> do
      return command

ehloAdvertisement :: StateT SessionState IO Reply
ehloAdvertisement = do
  let extensions = ["8BITMIME"]
  return $ reply' 250 (extensions ++ ["OK"])

ok :: Reply
ok = reply 250 "OK"

reject :: Reply
reject = reply 554 "Transaction failed"

sendReply :: Reply -> StateT SessionState IO ()
sendReply reply = do
  conn <- gets sessionConnection
  liftIO $ connSend conn (renderReply reply)
