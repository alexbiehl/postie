
module Web.Postie.Pipes(
    dataChunks
  , UnexpectedEndOfInputException
  , TooMuchDataException
  ) where

import Pipes
import Pipes.Parse

import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as BS

import Control.Monad (when, unless)
import Control.Exception (throw, Exception)

data UnexpectedEndOfInputException = UnexpectedEndOfInputException
  deriving (Show, Typeable)

data TooMuchDataException = TooMuchDataException
  deriving (Show, Typeable)

instance Exception UnexpectedEndOfInputException
instance Exception TooMuchDataException

dataChunks :: Int -> Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
dataChunks n p = chunks p >-> delimited n
  where
    delimited remaining | remaining <= 0 = throw UnexpectedEndOfInputException
    delimited remaining = do
      bs <- await
      when (BS.length bs > remaining) $ do
        throw TooMuchDataException
      case bs of
        "."  -> return ()
        _    -> yield bs >> delimited (remaining - BS.length bs)

chunks :: Producer BS.ByteString IO () -> Producer BS.ByteString IO ()
chunks = go
  where
    go p = do
      (bs, leftover) <- liftIO $ runStateT (drawThrow return) p

      case BS.elemIndex '\r' bs of
        Nothing -> do
          lo <- liftIO $ execStateT (drawThrow (unDraw . BS.append bs)) leftover
          go lo
        Just n  -> do
            let here = killCR $ BS.take n bs
                rest = killCR $ BS.drop (n + 1) bs
            lo <- case BS.null rest of
                    True  -> return leftover
                    False -> liftIO $ execStateT (unDraw rest) leftover
            unless (BS.null here) $ do
              yield here
            yield "\r\n" >> go lo

    drawThrow f = draw >>= maybe (throw UnexpectedEndOfInputException) f

    killCR bs
      | BS.null bs = bs
      | BS.last bs == '\n' = BS.init bs
      | otherwise = bs
