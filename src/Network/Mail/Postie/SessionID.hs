module Network.Mail.Postie.SessionID
  ( SessionID,
    -- | Unique session identifier
    mkSessionID,
    -- | Creates a SessionID
    toByteString,
    -- | Converts SessionID to ByteString
  )
where

import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.UUID (UUID, toASCIIBytes, toString)
import Data.UUID.V4 (nextRandom)

newtype SessionID = SessionID {toUUID :: UUID}
  deriving (Eq, Ord, Typeable)

instance Show SessionID where
  show = toString . toUUID

mkSessionID :: IO SessionID
mkSessionID = SessionID `fmap` nextRandom

toByteString :: SessionID -> ByteString
toByteString = toASCIIBytes . toUUID
