{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Builder.Reader.InternalSpec
  (
    main
  , spec
  ) where

import Data.Attoparsec.ByteString.Char8
import Data.Either
import Options.Applicative.Builder.Reader.Internal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Attoparsec parsers" $ do
    describe "Parse non-quoted value" $ do
      it "value with no space" $ do
        let result = parseOnly parseNonQuotedValue "value-with-no-space"
        result `shouldBe` Right "value-with-no-space"
      it "value with spaces" $ do
        let result = parseOnly parseNonQuotedValue "value with space"
        result `shouldBe` Right "value with space"
      it "empty value" $ do
        let result = parseOnly parseNonQuotedValue ""
        result `shouldBe` Right ""
      it "value with '\"'" $ do
        let result = parseOnly parseNonQuotedValue "value starting with '\"'"
        result `shouldBe` Right "value starting with '\"'"
    describe "Parse quoted char" $ do
      it "character 'a'" $ do
        let result = parseOnly parseQuotedChar "a"
        result `shouldBe` Right 'a'
      it "character '\\\"'" $ do
        let result = parseOnly parseQuotedChar "\\\""
        result `shouldBe` Right '"'
      it "character '\"'" $ do
        let result = parseOnly parseQuotedChar "\""
        isLeft result `shouldBe` True
    describe "Parse quoted values" $ do
      it "value with no escaped quote" $ do
        let result = parseOnly parseQuotedValue "\"value with no escaped quote\""
        result `shouldBe` Right "value with no escaped quote"
      it "value with escaped quotes" $ do
        let result = parseOnly parseQuotedValue "\"value with \\\"escaped\\\" quotes\""
        result `shouldBe` Right "value with \"escaped\" quotes"
      it "empty quoted value" $ do
        let result = parseOnly parseQuotedValue "\"\""
        result `shouldBe` Right ""
      it "value with missing starting quote" $ do
        let result = parseOnly parseQuotedValue "mising beginning quote\""
        isLeft result `shouldBe` True
      it "value with missing ending quote" $ do
        let result = parseOnly parseQuotedValue "\"missing ending quote"
        isLeft result `shouldBe` True
      it "value with premature ending quote" $ do
        let result = parseOnly parseQuotedValue "\"premature end of\" value"
        result `shouldBe` Right "premature end of"
    describe "Parse values" $ do
      it "non-quoted value" $ do
        let result = parseOnly parseValue "non-quoted value"
        result `shouldBe` Right "non-quoted value"
      it "empty non-quoted value" $ do
        let result = parseOnly parseValue ""
        result `shouldBe` Right ""
      it "quoted value" $ do
        let result = parseOnly parseValue "\"quoted value \\\"indeed\\\"\""
        result `shouldBe` Right "quoted value \"indeed\""
      it "empty quoted value" $ do
        let result = parseOnly parseQuotedValue "\"\""
        result `shouldBe` Right ""
      it "quoted value with missing ending quote" $ do
        let result = parseOnly parseValue "\"value"
        isLeft result `shouldBe` True
      it "quoted value with premature ending quote" $ do
        let result = parseOnly (parseValue <* endOfInput) "\"value is not \"over"
        isLeft result `shouldBe` True
    describe "Parse names" $ do
      it "valid name char" $ do
        isNameChar 'a' `shouldBe` True
        isNameChar 'z' `shouldBe` True
        isNameChar 'A' `shouldBe` True
        isNameChar 'B' `shouldBe` True
        isNameChar '-' `shouldBe` True
        isNameChar '_' `shouldBe` True
        isNameChar ' ' `shouldBe` False
      it "invalid name char" $ do
        isNameChar '@' `shouldBe` False
        isNameChar '*' `shouldBe` False
        isNameChar ' ' `shouldBe` False
        isNameChar '=' `shouldBe` False
      it "valid names" $ do
        parseOnly parseName "classname" `shouldBe` Right "classname"
        parseOnly parseName "CLASSNAME" `shouldBe` Right "CLASSNAME"
        parseOnly parseName "ClassName" `shouldBe` Right "ClassName"
        parseOnly parseName "Class2Name" `shouldBe` Right "Class2Name"
        parseOnly parseName "class-name" `shouldBe` Right "class-name"
        parseOnly parseName "class_name" `shouldBe` Right "class_name"
      it "invalid names" $ do
        isLeft (parseOnly parseName "2classname") `shouldBe` True
        isLeft (parseOnly parseName "-classname") ` shouldBe` True
        isLeft (parseOnly parseName "_classname") `shouldBe` True
        isLeft (parseOnly (parseName <* endOfInput) "class@name") `shouldBe` True
        isLeft (parseOnly (parseName <* endOfInput) "class name") `shouldBe` True
        isLeft (parseOnly (parseName <* endOfInput) "class=name") `shouldBe` True
      it "incomplete names" $ do
        parseOnly parseName "class@name" `shouldBe` Right "class"
        parseOnly parseName "class name" `shouldBe` Right "class"
        parseOnly parseName "class=name" `shouldBe` Right "class"
    describe "Parse named values" $ do
      it "named (unquoted)values" $ do
        parseOnly parseNamedValue "email=noel.geoff@gmail.com"
          `shouldBe` Right ("email", "noel.geoff@gmail.com")
      it "named (quoted) values" $ do
        parseOnly parseNamedValue "name=\"Geoffrey Noel\""
          `shouldBe` Right ("name", "Geoffrey Noel")
      it "named (quoted) values with escaped quotes" $ do
        parseOnly parseNamedValue "author=\"David \\\"Nuke\\\" Winter\""
          `shouldBe` Right ("author", "David \"Nuke\" Winter")
      it "named (quoted) values with mising ending quote" $ do
        isLeft (parseOnly parseNamedValue "name=\"Geoffrey Noel")
          `shouldBe` True
      it "named (quoted) values with premature ending quote" $ do
        isLeft (parseOnly parseNamedValue "author=\"David \"Nuke\" Winter\"")
          `shouldBe` True
