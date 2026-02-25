{-# OPTIONS -fmax-pmcheck-models=100 #-}

module Network.Mail.Postie.Protocol
  ( TlsStatus (..),
    AuthStatus (..),
    Mailbox,
    Event (..),
    Command (..),
    SmtpFSM,
    Reply,
    initSmtpFSM,
    step,
    reply,
    reply',
    renderReply,
    parseCommand,
    parseHelo,
    parseMailFrom,
  )
where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 hiding (match)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor (($>))
import Network.Mail.Postie.Address
import Prelude hiding (takeWhile)

data TlsStatus = Active | Forbidden | Permitted | Required deriving (Eq)

data AuthStatus = Authed | NoAuth | AuthRequired deriving (Eq)

data SessionState
  = Unknown
  | HaveHelo
  | HaveEhlo
  | HaveMailFrom
  | HaveRcptTo
  | HaveData
  | HaveQuit

type Mailbox = Address

data Event
  = SayHelo BS.ByteString
  | SayHeloAgain BS.ByteString
  | SayEhlo BS.ByteString
  | SayEhloAgain BS.ByteString
  | SayOK
  | SetMailFrom Mailbox
  | AddRcptTo Mailbox
  | StartData
  | WantTls
  | WantAuth BS.ByteString
  | WantReset
  | WantQuit
  | TlsAlreadyActive
  | TlsNotSupported
  | NeedStartTlsFirst
  | NeedAuthFirst
  | NeedHeloFirst
  | NeedMailFromFirst
  | NeedRcptToFirst
  deriving (Eq, Show)

data Command
  = Helo BS.ByteString
  | Ehlo BS.ByteString
  | MailFrom Mailbox
  | RcptTo Mailbox
  | StartTls
  | Auth BS.ByteString
  | Data
  | Rset
  | Quit
  | Noop
  deriving (Eq, Show)

newtype SmtpFSM = SmtpFSM {step :: Command -> TlsStatus -> AuthStatus -> (Event, SmtpFSM)}

initSmtpFSM :: SmtpFSM
initSmtpFSM = SmtpFSM (handleSmtpCmd Unknown)

handleSmtpCmd :: SessionState -> Command -> TlsStatus -> AuthStatus -> (Event, SmtpFSM)
handleSmtpCmd st cmd tlsSt auth = match tlsSt auth st cmd
  where
    match :: TlsStatus -> AuthStatus -> SessionState -> Command -> (Event, SmtpFSM)
    match _ _ HaveQuit _ = undefined
    match _ _ HaveData Data = undefined
    match _ _ _ Quit = trans (HaveQuit, WantQuit)
    match _ _ Unknown (Helo x) = trans (HaveHelo, SayHelo x)
    match _ _ _ (Helo x) = trans (HaveHelo, SayHeloAgain x)
    match _ _ Unknown (Ehlo x) = trans (HaveEhlo, SayEhlo x)
    match _ _ _ (Ehlo x) = trans (HaveEhlo, SayEhloAgain x)
    match Required _ _ (MailFrom _) = event NeedStartTlsFirst
    match _ AuthRequired _ (MailFrom _) = event NeedAuthFirst
    match _ _ Unknown (MailFrom _) = event NeedHeloFirst
    match _ _ _ (MailFrom x) = trans (HaveMailFrom, SetMailFrom x)
    match Required _ _ (RcptTo _) = event NeedStartTlsFirst
    match _ AuthRequired _ (RcptTo _) = event NeedAuthFirst
    match _ _ Unknown (RcptTo _) = event NeedHeloFirst
    match _ _ HaveHelo (RcptTo _) = event NeedMailFromFirst
    match _ _ HaveEhlo (RcptTo _) = event NeedMailFromFirst
    match _ _ _ (RcptTo x) = trans (HaveRcptTo, AddRcptTo x)
    match Required _ _ Data = event NeedStartTlsFirst
    match _ AuthRequired _ Data = event NeedAuthFirst
    match _ _ Unknown Data = event NeedHeloFirst
    match _ _ HaveHelo Data = event NeedMailFromFirst
    match _ _ HaveEhlo Data = event NeedMailFromFirst
    match _ _ HaveMailFrom Data = event NeedRcptToFirst
    match _ _ HaveRcptTo Data = trans (HaveData, StartData)
    match Required _ _ Rset = event NeedStartTlsFirst
    match _ _ _ Rset = trans (HaveHelo, WantReset)
    match Active _ _ StartTls = event TlsAlreadyActive
    match Forbidden _ _ StartTls = event TlsNotSupported
    match _ _ _ StartTls = trans (Unknown, WantTls)
    match Required _ _ (Auth _) = event NeedStartTlsFirst
    match _ _ _ (Auth d) = trans (HaveEhlo, WantAuth d)
    match _ _ _ Noop = event SayOK

    event :: Event -> (Event, SmtpFSM)
    event e = (e, SmtpFSM (handleSmtpCmd st))

    trans :: (SessionState, Event) -> (Event, SmtpFSM)
    trans (st', e) = (e, SmtpFSM (handleSmtpCmd st'))

type StatusCode = Int

data Reply = Reply StatusCode [LBS.ByteString]

reply :: StatusCode -> LBS.ByteString -> Reply
reply c s = reply' c [s]

reply' :: StatusCode -> [LBS.ByteString] -> Reply
reply' = Reply

renderReply :: Reply -> LBS.ByteString
renderReply (Reply code msgs) = LBS.concat msg'
  where
    prefixCon = LBS.pack (show code ++ "-")
    prefixEnd = LBS.pack (show code ++ " ")
    fmt p l = LBS.concat [p, l, "\r\n"]
    (x : xs) = reverse msgs
    msgCon = map (fmt prefixCon) xs
    msgEnd = fmt prefixEnd x
    msg' = reverse (msgEnd : msgCon)

parseCommand :: Parser Command
parseCommand = commands <* crlf
  where
    commands =
      choice
        [ parseQuit,
          parseData,
          parseRset,
          parseHelo,
          parseEhlo,
          parseStartTls,
          parseAuth,
          parseMailFrom,
          parseRcptTo,
          parseNoop
        ]

crlf :: Parser ()
crlf = void $ char '\r' >> char '\n'

parseHello :: (BS.ByteString -> Command) -> BS.ByteString -> Parser Command
parseHello f s = f `fmap` parser
  where
    parser = stringCI s *> char ' ' *> takeWhile (notInClass "\r ")

parseHelo :: Parser Command
parseHelo = parseHello Helo "helo"

parseEhlo :: Parser Command
parseEhlo = parseHello Ehlo "ehlo"

parseMailFrom :: Parser Command
parseMailFrom = stringCI "mail from:<" *> (MailFrom `fmap` addrSpec) <* char '>'

parseRcptTo :: Parser Command
parseRcptTo = stringCI "rcpt to:<" *> (RcptTo `fmap` addrSpec) <* char '>'

parseStartTls :: Parser Command
parseStartTls = stringCI "starttls" $> StartTls

parseAuth :: Parser Command
parseAuth = Auth <$> (stringCI "auth plain" *> char ' ' *> takeWhile (notInClass "\r "))

parseRset :: Parser Command
parseRset = stringCI "rset" $> Rset

parseData :: Parser Command
parseData = stringCI "data" $> Data

parseQuit :: Parser Command
parseQuit = stringCI "quit" $> Quit

parseNoop :: Parser Command
parseNoop = do
  void $ stringCI "noop"
  -- Per RFC 5321 §4.1.1.9, NOOP should ignore any parameter string specified
  skipWhile (notInClass "\r\n")
  pure Noop
