
module Web.Postie.Types where

import Data.ByteString (ByteString)

import qualified Data.Text as T
import qualified Pipes as P

data HandlerResponse = Accepted | Rejected

data Mail = Mail {
    mailSender     :: T.Text
  , mailRecipients :: [T.Text]
  , mailBody       :: P.Producer ByteString IO ()
  }

type Application = Mail -> IO HandlerResponse
