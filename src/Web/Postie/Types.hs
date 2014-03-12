
module Web.Postie.Types where

import Web.Postie.Address

import Data.ByteString (ByteString)
import Pipes (Producer)

data HandlerResponse = Accepted | Rejected

data Mail = Mail {
    mailSender     :: Address
  , mailRecipients :: [Address]
  , mailBody       :: Producer ByteString IO ()
  }

type Application = Mail -> IO HandlerResponse
