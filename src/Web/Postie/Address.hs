
module Web.Postie.Address(
    Address
  , addressLocalPart
  , addressDomain
  , toByteString

  , addrSpec
  ) where

import Data.String
import Data.Typeable (Typeable)
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS

import Control.Applicative

data Address = Address {
    addressLocalPart :: BS.ByteString
  , addressDomain    :: BS.ByteString
  }
  deriving (Eq, Ord, Typeable)

instance Show Address where
  show = show . toByteString

instance IsString Address where
  fromString = either (error "invalid email literal") id . parseOnly addrSpec . BS.pack

toByteString :: Address -> BS.ByteString
toByteString (Address l d) = BS.concat [l, BS.singleton '@', d]

-- Borrowed form email-validate-2.0.1

addrSpec = do
	localPart <- local
	char '@'
	domainPart <- domain
	return (Address localPart domainPart)

local = dottedAtoms
domain = dottedAtoms <|> domainLiteral

dottedAtoms = BS.intercalate (BS.singleton '.') <$>
	(optional cfws *> (atom <|> quotedString) <* optional cfws)	`sepBy1` (char '.')
atom = takeWhile1 isAtomText

isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x

domainLiteral = (BS.cons '[' . flip BS.snoc ']' . BS.concat) <$> (between (optional cfws *> char '[') (char ']' <* optional cfws) $
	many (optional fws >> takeWhile1 isDomainText) <* optional fws)
isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x

quotedString = (\x -> BS.concat $ [BS.singleton '"', BS.concat x, BS.singleton '"']) <$> (between (char '"') (char '"') $
	many (optional fws >> quotedContent) <* optional fws)

quotedContent = takeWhile1 isQuotedText <|> quotedPair
isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

cfws = ignore $ many (comment <|> fws)

fws :: Parser ()
fws = ignore $
	ignore (wsp1 >> optional (crlf >> wsp1))
	<|> ignore (many1 (crlf >> wsp1))

ignore :: Parser a -> Parser ()
ignore x = x >> return ()

between l r x = l *> x <* r

comment :: Parser ()
comment = ignore ((between (char '(') (char ')') $
	many (ignore commentContent <|> fws)))

commentContent = skipWhile1 isCommentText <|> ignore quotedPair <|> comment
isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

nullChar = char '\0'

skipWhile1 x = satisfy x >> skipWhile x

wsp1 = skipWhile1 isWsp
wsp = satisfy isWsp
isWsp x = x == ' ' || x == '\t'

isAlphaNum x = isDigit x || isAlpha_ascii x
cr = char '\r'
lf = char '\n'
crlf = cr >> lf >> return ()

isVchar = inClass "\x21-\x7e"
vchar = satisfy isVchar

isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"
obsNoWsCtl = satisfy isObsNoWsCtl
