
module Web.Postie.Protocol(
    TlsStatus(..)
  , Mailbox
  , Event(..)
  , Command(..)
  , SmtpFSM
  , Reply
  , initSmtpFSM
  , step
  , reply
  , reply'
  , renderReply
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data TlsStatus = Active | Forbidden | Permitted | Required deriving (Eq)

data SessionState = Unknown
                  | HaveHelo
                  | HaveEhlo
                  | HaveMailFrom
                  | HaveRcptTo
                  | HaveData
                  | HaveQuit

type Mailbox = BS.ByteString

data Event =  SayHelo BS.ByteString
           | SayHeloAgain BS.ByteString
           | SayEhlo BS.ByteString
           | SayEhloAgain BS.ByteString
           | SayOK
           | SetMailFrom Mailbox
           | AddRcptTo Mailbox
           | StartData
           | WantTls
           | WantReset
           | WantQuit
           | TlsAlreadyActive
           | TlsNotSupported
           | NeedStartTlsFirst
           | NeedHeloFirst
           | NeedMailFromFirst
           | NeedRcptToFirst
           deriving (Eq, Show)

data Command = Helo BS.ByteString
             | Ehlo BS.ByteString
             | MailFrom Mailbox
             | RcptTo Mailbox
             | StartTls
             | Data
             | Rset
             | Quit

newtype SmtpFSM = SmtpFSM { step :: Command -> TlsStatus -> (Event, SmtpFSM) }

initSmtpFSM :: SmtpFSM
initSmtpFSM = SmtpFSM (handleSmtpCmd Unknown)

handleSmtpCmd :: SessionState -> Command -> TlsStatus -> (Event, SmtpFSM)
handleSmtpCmd st cmd tlsSt = match tlsSt st cmd
  where
    match :: TlsStatus -> SessionState -> Command -> (Event, SmtpFSM)
    match _         HaveQuit  _            = undefined
    match _         HaveData  Data         = undefined
    match _         _         Quit         = trans (HaveQuit, WantQuit)
    match _         Unknown   (Helo x)     = trans (HaveHelo, SayHelo x)
    match _         _         (Helo x)     = event (SayHeloAgain x)
    match _         Unknown   (Ehlo x)     = trans (HaveEhlo, SayEhlo x)
    match _         _         (Ehlo x)     = event (SayEhloAgain x)
    match Required  _         (MailFrom _) = event NeedStartTlsFirst
    match _         Unknown   (MailFrom _) = event NeedHeloFirst
    match _         _         (MailFrom x) = trans (HaveMailFrom, SetMailFrom x)
    match Required  _         (RcptTo _)   = event NeedStartTlsFirst
    match _         Unknown   (RcptTo _)   = event NeedHeloFirst
    match _         HaveHelo  (RcptTo _)   = event NeedMailFromFirst
    match _         HaveEhlo  (RcptTo _)   = event NeedMailFromFirst
    match _         _         (RcptTo x)   = trans (HaveRcptTo, AddRcptTo x)
    match Required  _            Data      = event NeedStartTlsFirst
    match _         Unknown      Data      = event NeedHeloFirst
    match _         HaveHelo     Data      = event NeedMailFromFirst
    match _         HaveEhlo     Data      = event NeedMailFromFirst
    match _         HaveMailFrom Data      = event NeedRcptToFirst
    match _         HaveRcptTo   Data      = trans (HaveData, StartData)
    match Required  _           Rset       = event NeedStartTlsFirst
    match _         _           Rset       = trans (HaveHelo, WantReset)
    match Active    _           StartTls   = event TlsAlreadyActive
    match Forbidden _           StartTls   = event TlsNotSupported
    match _         _           StartTls   = trans (Unknown, WantTls)

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
    (x:xs) = reverse msgs
    msgCon = map (fmt prefixCon) xs
    msgEnd = fmt prefixEnd x
    msg' = reverse (msgEnd:msgCon)
