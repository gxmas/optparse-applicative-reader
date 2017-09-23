{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Builder.Reader.Internal where

import           Control.Applicative ((<|>), many)
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (cons, pack, unpack)

type NamedValue = (String, String)

parseNamedValue :: Parser NamedValue
parseNamedValue = (,) <$> parseName <* A.char '=' <*> parseValue <* A.endOfInput
{-# INLINABLE parseNamedValue #-}

-- |Predicate to test if a character is valid `name`.  Note that the
-- first letter of a `name` must be an ascii letter.
isNameChar :: Char -> Bool
isNameChar c = A.isAlpha_ascii c || A.isDigit c || c == '-' || c == '_'
{-# INLINABLE isNameChar #-}

-- |Parse the `name` of a `NamedValue`
parseName :: Parser String
parseName = B.unpack <$> (B.cons <$> A.satisfy A.isAlpha_ascii <*> (A.takeWhile isNameChar))
{-# INLINABLE parseName #-}

parseValue :: Parser String
parseValue = do
  cM <- A.peekChar
  case cM of
    Nothing -> return ""
    Just c  -> if c == '"' then parseQuotedValue else parseNonQuotedValue
{-# INLINABLE parseValue #-}

parseQuotedChar :: Parser Char
parseQuotedChar = (A.string "\\\"" *> pure '"') <|> A.satisfy (/= '"')
{-# INLINABLE parseQuotedChar #-}

parseQuotedValue :: Parser String
parseQuotedValue = A.char '"' *> many parseQuotedChar <* A.char '"'
{-# INLINABLE parseQuotedValue #-}

parseNonQuotedValue :: Parser String
parseNonQuotedValue = B.unpack <$> A.takeByteString
{-# INLINABLE parseNonQuotedValue #-}
