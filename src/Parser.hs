{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import AST (Argument (..), Flag (..), FlagArgument (..), FlagDefinition (..), Include (..), OptionSpecifier (OptionSpecifierConst, OptionSpecifierShell), OptionsDefinition (OptionsDefinition), Ref (..))
import Control.Monad (void)
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe (isJust)
import Lang (lexeme, parens, stringLiteral)
import qualified Lang
import Text.Parsec (Parsec, ParsecT, alphaNum, many1, noneOf, sepBy, space)
import Text.Parsec.Char (char)
import Text.Parsec.Token (GenTokenParser (stringLiteral), symbol)
import Text.ParserCombinators.Parsec
  ( GenParser,
    alphaNum,
    between,
    eof,
    many,
    manyTill,
    optionMaybe,
    spaces,
    string,
    (<|>),
  )

--   root    - root file
--   cmd     - command
--   sub     - subcommand
-- X flag    - flag
-- X flagarg - flag argument
-- X include - inclusion?
--   defargs - arguments definition
-- X defopts - options definition
-- X opts    - option specifier
-- X opt     - optional prefix
-- X reqd    - required prefix
-- X arg     - argument
-- X desc    - description
-- X title   - title
-- X ref     - reference
--   varargs - variable args

parseEither pa pb = (Left <$> pa) <|> (Right <$> pb)

parseOptionalParens pa = parens pa <|> pa

parseRef = fmap Ref Lang.identifier

parseDescription = do
  _ <- Lang.lexeme (Lang.symbol "desc")
  Lang.lexeme Lang.stringLiteral

parseTitle = do
  _ <- Lang.lexeme (Lang.symbol "title")
  Lang.lexeme Lang.stringLiteral

parseSplitOn :: GenParser Char st b -> GenParser Char st a -> GenParser Char st [[a]]
parseSplitOn sep p = manyTill uptoSEP eof
  where
    uptoSEP = manyTill p (eof <|> (sep >> return ()))

parseFlag =
  let f = sepBy (many alphaNum) (string ",")
   in fmap Flag f

parseFlagDefinition = do
  _ <- string "flag"
  _ <- spaces
  flags <- parseOptionalParens $ sepBy parseFlag (many1 space)
  desc <- optionMaybe Lang.stringLiteral
  return FlagDefinition {flagDefinitionFlags = flags, flagDefinitionDescription = desc}

parseNamesSepBySpace =
  sepBy (many1 (alphaNum <|> char '-')) (many1 space)

parseOptionSpecifier =
  Lang.braces
    ( do
        _ <- Lang.lexeme (string "opts")
        mode <- parseEither (Lang.lexeme $ string "shell") (Lang.lexeme $ string "const")
        case mode of
          Left _ -> fmap OptionSpecifierShell Lang.stringLiteral
          Right _ -> fmap OptionSpecifierConst (parens parseNamesSepBySpace)
    )

parseOptionsDefinition = do
  _ <- Lang.lexeme (Lang.symbol "defopts")
  id <- parseRef
  specifier <- parseOptionSpecifier
  return (OptionsDefinition (coerce id) specifier)

parseFlagArgOptions = parseEither (Lang.lexeme parseRef) (Lang.lexeme parseOptionSpecifier)

parseFlagArg = do
  required <- Lang.lexeme (optionMaybe (Lang.symbol "reqd"))
  _ <- Lang.lexeme (Lang.symbol "flagarg")
  flag <- Lang.lexeme parseFlag
  desc <- optionMaybe (Lang.lexeme Lang.stringLiteral)
  options <- optionMaybe parseFlagArgOptions
  return
    FlagArgument
      { flagArgumentNames = flag,
        flagArgumentDescription = desc,
        flagArgumentOptionSpecifier = options,
        flagArgumentRequired = isJust required
      }

parseArg = do
  optional <- optionMaybe $ Lang.lexeme $ Lang.symbol "opt"
  _ <- Lang.lexeme $ Lang.symbol "arg"
  names <- optionMaybe (Lang.lexeme (fmap return (many1 alphaNum) <|> Lang.parens parseNamesSepBySpace))
  desc <- optionMaybe (Lang.lexeme Lang.stringLiteral)
  optsReference <- optionMaybe parseOptionSpecifier
  return
    Argument
      { argumentOptional = isJust optional,
        argumentNames = names,
        argumentDescription = desc,
        argumentOptionSpecifier = optsReference
      }

-- parseInclude :: GenParser Char st Include
-- parseInclude = do
--   _ <- string "include "
--   _ <- spaces
--   ref <- parseRef
--   return (Include ref)
