module Web.Postie.Parser where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8

dataEnd :: Parser ()
dataEnd = string "\r\n.\r\n" >> return ()

data DataState = R0 | N0 | DOT | R1 | N1

dataParser :: Parser BS.ByteString
dataParser = scan R0 f
  where
    f R0   '\r'   = Just N0
    f N0   '\n'   = Just DOT
    f DOT  '.'    = Just R1
    f R1   '\r'   = Just N1
    f N1   '\n'   = Nothing
    f _    _      = Just R0
