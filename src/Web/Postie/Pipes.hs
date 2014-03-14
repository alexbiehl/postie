
module Web.Postie.Pipes(
    dataChunks
  , attoParser
  , UnexpectedEndOfInputException
  , TooMuchDataException
  ) where

import Prelude hiding (lines)

import Pipes
import Pipes.Parse

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec as AT

import Control.Monad (when, unless)
import Control.Applicative
import Control.Exception (throw, Exception)

data UnexpectedEndOfInputException = UnexpectedEndOfInputException
  deriving (Show, Typeable)

data TooMuchDataException = TooMuchDataException
  deriving (Show, Typeable)

instance Exception UnexpectedEndOfInputException
instance Exception TooMuchDataException

attoParser :: AT.Parser r -> Parser BS.ByteString IO (Maybe r)
attoParser p = do
    result <- AT.parseWith draw' p ""
    case result of
      AT.Done t r -> do
                      unless (BS.null t) (unDraw t)
                      return (Just r)
      _           -> return Nothing
  where
    draw' = fromMaybe "" <$> draw

dataChunks :: Int -> Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
dataChunks n p = lines p >-> go n
  where
    go remaining | remaining <= 0 = throw UnexpectedEndOfInputException
    go remaining = do
      bs <- await
      unless (bs == ".") $ do
        yield (unescape bs)
        yield "\r\n"
        go (remaining - BS.length bs - 2)

    unescape bs | BS.null bs                            = bs
                | BS.head bs == '.' && BS.length bs > 1 = BS.tail bs
                | otherwise                             = bs

lines :: Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
lines = go
  where
    go p = do
      (line, leftover) <- lift $ runStateT lineParser p
      yield line
      go leftover

lineParser :: Parser BS.ByteString IO BS.ByteString
lineParser = go id
  where
    go f = do
      bs <- maybe (throw UnexpectedEndOfInputException) (return . f) =<< draw
      case BS.elemIndex '\r' bs of
        Nothing -> go (BS.append bs)
        Just n  -> do
          let here = killCR $ BS.take n bs
              rest = BS.drop (n + 1) bs
          unDraw rest
          return here

    killCR bs
      | BS.null bs = bs
      | BS.head bs == '\n' || BS.head bs == '\r' = killCR $ BS.tail bs
      | BS.last bs == '\n' || BS.last bs == '\r' = killCR $ BS.init bs
      | otherwise = bs
