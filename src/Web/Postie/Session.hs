
module Web.Postie.Session(
    runSession
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

import qualified Data.ByteString.Char8 as BS

import qualified Pipes.Parse as P

import Control.Applicative
import Control.Monad.State

data SessionState = SessionState {
    sessionID               :: SessionID
  , sessionApp              :: Application
  , sessionSettings         :: Settings
  , sessionConnection       :: Connection
  , sessionConnectionInput  :: P.Producer BS.ByteString IO ()
  , sessionProtocolState    :: SMTP.SmtpFSM
  , sessionTransaction      :: Transaction
  }

data Transaction = TxnInitial
                 | TxnHaveMailFrom Address
                 | TxnHaveRecipient Address [Address]

runSession :: SessionID -> Settings -> Connection -> Application -> IO ()
runSession sid settings connection app = evalStateT startSession session
  where
    session = SessionState {
      sessionID              = sid
    , sessionApp             = app
    , sessionSettings        = settings
    , sessionConnection      = connection
    , sessionConnectionInput = connectionP connection
    , sessionProtocolState   = SMTP.initSmtpFSM
    , sessionTransaction     = TxnInitial
    }

startSession :: StateT SessionState IO ()
startSession = do
  sendReply $ reply 220 "hello!"
  sessionLoop

sessionLoop :: StateT SessionState IO ()
sessionLoop = do
    (event, fsm') <- SMTP.step <$> getSmtpFsm <*> getCommand <*> getTlsStatus
    case event of
      WantQuit -> do
        sendReply $ reply 221 "goodbye"
        return ()
      _        -> do
        modify (\ss -> ss { sessionProtocolState = fsm' })
        handleEvent event >> sessionLoop
  where
    getSmtpFsm   = gets sessionProtocolState
    getTlsStatus = do
      conn <- gets sessionConnection

      return $ case conn of
        _ | connIsSecure conn       -> SMTP.Active
          | connDemandStartTLS conn -> SMTP.Required
          | connAllowStartTLS  conn -> SMTP.Permitted
          | otherwise               -> SMTP.Forbidden

handleEvent :: SMTP.Event -> StateT SessionState IO ()
handleEvent (SayHelo x)      = do
  sid     <- gets sessionID
  handler <- settingsOnHello <$> gets sessionSettings
  result  <- liftIO $ handler sid x
  handlerResponse result (sendReply ok)

handleEvent (SayEhlo x)      = do
  sid     <- gets sessionID
  handler <- settingsOnHello <$> gets sessionSettings
  result  <- liftIO $ handler sid x
  handlerResponse result $ do
    sendReply =<< ehloAdvertisement

handleEvent (SayEhloAgain _) = sendReply ok
handleEvent (SayHeloAgain _) = sendReply ok
handleEvent SayOK            = sendReply ok

handleEvent (SetMailFrom x)  = do
  sid     <- gets sessionID
  handler <- settingsOnMailFrom <$> gets sessionSettings
  result  <- liftIO $ handler sid x
  handlerResponse result $ do
    modify (\ss -> ss { sessionTransaction = TxnHaveMailFrom x })
    sendReply ok

handleEvent (AddRcptTo x)   = do
  sid     <- gets sessionID
  handler <- settingsOnRecipient <$> gets sessionSettings
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
    (TxnHaveRecipient sender recipients) <- gets sessionTransaction
    sid    <- gets sessionID
    mail   <- Mail sid sender recipients <$> chunks
    app    <- gets sessionApp
    result <- liftIO $ app mail
    handlerResponse result $ do
      sendReply ok
      modify (\ss -> ss { sessionTransaction = TxnInitial })
  where
    maxDataLength     = settingsMaxDataSize `fmap` gets sessionSettings
    chunks            = dataChunks <$> maxDataLength <*> gets sessionConnectionInput

handleEvent WantTls = do
  sid     <- gets sessionID
  handler <- settingsOnStartTLS <$> gets sessionSettings
  liftIO $ handler sid
  sendReply ok
  conn    <- gets sessionConnection
  conn'   <- liftIO $ connStartTls conn
  modify (\ss -> ss {
    sessionConnection      = conn'
  , sessionConnectionInput = connectionP conn'
  , sessionTransaction     = TxnInitial
  })

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

handleEvent _ = error "impossible"

handlerResponse :: HandlerResponse -> StateT SessionState IO () -> StateT SessionState IO ()
handlerResponse Accepted action = action
handlerResponse Rejected _      = sendReply reject

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
    stls <- startTls
    let extensions = ["8BITMIME"] ++ stls
    return $ reply' 250 (extensions ++ ["OK"])
  where
    startTls = do
      conn <- gets sessionConnection
      if (not $ connIsSecure conn) &&
         (connAllowStartTLS conn) ||
         (connDemandStartTLS conn)
        then
          return ["STARTTLS"]
          else return []

ok :: Reply
ok = reply 250 "OK"

reject :: Reply
reject = reply 554 "Transaction failed"

sendReply :: Reply -> StateT SessionState IO ()
sendReply r = do
  conn <- gets sessionConnection
  liftIO $ connSend conn (renderReply r)
