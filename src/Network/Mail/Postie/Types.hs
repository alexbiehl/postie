module Network.Mail.Postie.Types
  ( HandlerResponse (..),
    Mail (..),
    Application,
  )
where

import Data.ByteString (ByteString)
import Pipes (Producer)
import Network.Mail.Postie.Address
import Network.Mail.Postie.SessionID (SessionID)

-- | Handler response indicating validity of email transaction.
data HandlerResponse
  = -- | Accepted, allow further processing.
    Accepted
  | -- | Rejected, stop transaction.
    Rejected

-- | Received email
data Mail
  = Mail
      { mailSessionID :: SessionID,
        mailAuth :: Maybe ByteString,
        -- | Sender
        mailSender :: Address,
        -- | Recipients
        mailRecipients :: [Address],
        -- | Mail content
        mailBody :: Producer ByteString IO ()
      }

-- | Application which receives Mails from postie
--   An Application has to fully consume the mailBody part of a mail, the behaviour is undefined if not.
type Application = Mail -> IO HandlerResponse
