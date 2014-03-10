
module Web.Postie.Session where

import Web.Postie.Types
import Web.Postie.Settings
import Web.Postie.Connection
import Web.Postie.SMTP (Event(..), Reply, reply, renderReply)
import qualified Web.Postie.SMTP as SMTP

import qualified Data.ByteString as BS
import qualified Pipes as P
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as P

import qualified Data.Attoparsec.Char8 as AT hiding (Parser)
import qualified Data.Attoparsec.Types as AT

import Control.Applicative
import Control.Monad.State

data SessionState = SessionState {
    sessionApp              :: Application
  , sessionSettings         :: Settings
  , sessionConnection       :: Connection
  , sessionConnectionInput  :: P.Producer BS.ByteString IO ()
  , sessionTLSStatus        :: SMTP.TlsStatus
  , sessionProtocolState    :: SMTP.SmtpFSM
  }

smtpCommandParser :: AT.Parser BS.ByteString SMTP.Command
smtpCommandParser = undefined

session :: StateT SessionState IO ()
session = do
    (event, fsm') <- SMTP.step <$> getSmtpFsm <*> getCommand <*> getTlsStatus
    case event of
      WantQuit -> do
        sendReply $ reply 221 "goodbye"
        return ()
      _        -> do
        modify (\ss -> ss { sessionProtocolState = fsm' })
        handleEvent event
        session
  where
    getSmtpFsm   = gets sessionProtocolState
    getTlsStatus = gets sessionTLSStatus

    getCommand = do
      input            <- gets sessionConnectionInput
      (result, input') <- liftIO $ P.runStateT (P.parse smtpCommandParser) input
      modify (\ss -> ss { sessionConnectionInput = input' })
      case result of
        Left _        -> do
          sendReply $ reply 500 "Syntax error, command unrecognized"
          getCommand
        Right command -> return command

handleEvent :: SMTP.Event -> StateT SessionState IO ()
handleEvent SayOK     = sendReply ok
handleEvent StartData = do
    sendReply $ reply 354 "End data with <CR><LF>.<CR><LF>"
    mail   <- Mail <$> pure "" <*> pure [""] <*> gets sessionConnectionInput
    app    <- gets sessionApp
    result <- liftIO $ app mail
    case result of
      Accepted -> sendReply ok
      Rejected -> sendReply $ reply 554 "message rejected"
  where
    



ok :: Reply
ok = reply 250 "OK"

sendReply :: Reply -> StateT SessionState IO ()
sendReply reply = do
  conn <- gets sessionConnection
  liftIO $ connSend conn (renderReply reply)
