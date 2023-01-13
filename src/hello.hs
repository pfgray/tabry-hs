module Main (main) where

import Data.String
import Parser (parseFlagArg, parseFlagDefinition, parseOptionalParens, parseRef, parseSplitOn)
import Text.Parsec (alphaNum, many, many1, parseTest, sepBy, space, spaces, string)

main :: IO ()
main = parseTest parseFlagArg (fromString "reqd flagarg region \"The region that you should join\" { opts const (us-east-1 us-west-2 ap-southeast-2 ap-southeast-1 ca-central-1 eu-central-1 eu-west-1) }")

--  (sepBy parseFlagValue (many1 space)) "aasdf,A    yooo,Y  yooo,Y yooo,Y"
