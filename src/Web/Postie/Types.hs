
module Web.Postie.Types(
    HandlerResponse(..)
  , Mail(..)
  , Application
  ) where

import Web.Postie.Address
import Web.Postie.SessionID (SessionID)

import Data.ByteString (ByteString)

import Pipes (Producer)

-- | Handler response indicating validity of email transaction.
data HandlerResponse = Accepted -- ^ Accepted, allow further processing.
                     | Rejected  -- ^ Rejected, stop transaction.

-- | Received email
data Mail = Mail {
    mailSessionID  :: SessionID
  , mailSender     :: Address -- ^ Sender
  , mailRecipients :: [Address]  -- ^ Recipients
  , mailBody       :: Producer ByteString IO () -- ^ Mail content
  }

-- | Application which receives Mails from postie
--   An Application has to fully consume the mailBody part of a mail, the behaviour is undefined if not.
type Application = Mail -> IO HandlerResponse
