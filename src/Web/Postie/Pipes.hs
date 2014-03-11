
module Web.Postie.Pipes(
    dataChunks
  , UnexpectedEndOfInputException
  , TooMuchDataException
  ) where

import Prelude hiding (lines)

import Pipes
import Pipes.Parse

import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as BS

import Control.Monad (when, unless)
import Control.Applicative
import Control.Exception (throw, Exception)

data UnexpectedEndOfInputException = UnexpectedEndOfInputException
  deriving (Show, Typeable)

data TooMuchDataException = TooMuchDataException
  deriving (Show, Typeable)

instance Exception UnexpectedEndOfInputException
instance Exception TooMuchDataException

dataChunks :: Int -> Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
dataChunks n p = lines p >-> go n
  where
    go remaining | remaining <= 0 = throw UnexpectedEndOfInputException
    go remaining = do
      bs <- await
      when (BS.length bs > remaining) $ do
        throw TooMuchDataException
      unless (bs == ".") $ do
        yield bs >> yield "\r\n" >> go (remaining - BS.length bs - 2)

lines :: Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
lines = go
  where
    go p = do
      (line, leftover) <- liftIO $ runStateT lineParser p
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
              rest = killCR $ BS.drop (n + 1) bs
          unDraw rest
          return here

    killCR bs
      | BS.null bs = bs
      | BS.last bs == '\n' = BS.init bs
      | otherwise = bs
