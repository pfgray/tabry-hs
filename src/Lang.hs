module Lang where

import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.ByteString
import Text.Parsec (ParsecT, alphaNum, char, oneOf)
import Text.Parsec.ByteString
import Text.Parsec.Char (letter)
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser, makeTokenParser)
import qualified Text.Parsec.Token as P

lang :: GenLanguageDef ByteString u Identity
lang = LanguageDef "" "" "#" False (char '@') (alphaNum <|> char '-') (oneOf "") (oneOf "") [] [] True

lexer :: GenTokenParser ByteString u Identity
lexer = makeTokenParser lang

identifier = P.identifier lexer

reserved = P.reserved lexer

operator = P.operator lexer

reservedOp = P.reservedOp lexer

charLiteral = P.charLiteral lexer

stringLiteral = P.stringLiteral lexer

natural = P.natural lexer

integer = P.integer lexer

float = P.float lexer

naturalOrFloat = P.naturalOrFloat lexer

decimal = P.decimal lexer

hexadecimal = P.hexadecimal lexer

octal = P.octal lexer

symbol = P.symbol lexer

lexeme = P.lexeme lexer

whiteSpace = P.whiteSpace lexer

parens = P.parens lexer

braces = P.braces lexer

angles = P.angles lexer

brackets = P.brackets lexer

semi = P.semi lexer

comma = P.comma lexer

colon = P.colon lexer

dot = P.dot lexer

semiSep = P.semiSep lexer

semiSep1 = P.semiSep1 lexer

commaSep = P.commaSep lexer

commaSep1 = P.commaSep1 lexer

--     (LanguageDef "" "" "#" False (letter <|> char '@') (alphaNum <|> char '_') (oneOf "") (oneOf "") [] [] True)

--       String
-- -> String
-- -> String
-- -> Bool
-- -> ParsecT s u m Char
-- -> ParsecT s u m Char
-- -> ParsecT s u m Char
-- -> ParsecT s u m Char
-- -> [String]
-- -> [String]
-- -> Bool