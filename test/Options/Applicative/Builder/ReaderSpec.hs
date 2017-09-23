{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Builder.ReaderSpec (main , spec) where

import Data.Either
import Options.Applicative
import Options.Applicative.Builder.Reader
import Options.Applicative.Internal
import Test.Hspec

instance Show ParseError where
  show pe =
    case pe of
      ErrorMsg e       -> "(ErrorMsg " ++ e ++ ")"
      InfoMsg e        -> "(InfoMsg " ++ e ++ ")"
      ShowHelpText     -> "(ShowHelpText)"
      UnknownError     -> "(UnknownError)"
      MissingError e p -> "(MissingError " ++ show e ++ " _)"

instance Eq ParseError where
  l == r = case (l,r) of
    ((ErrorMsg le), (ErrorMsg re))           -> le == re
    ((InfoMsg le), (InfoMsg re))             -> le == re
    (ShowHelpText, ShowHelpText)             -> True
    (UnknownError, UnknownError)             -> True
    ((MissingError _ _), (MissingError _ _)) -> True
    _                                        -> False

main :: IO ()
main = hspec spec

testReadM :: ReadM a -> String -> Either ParseError a
testReadM r s = fst $ runP (runReadM r s) defaultPrefs

spec :: Spec
spec = do
  describe "ReadM NamedValue" $ do
    it "name with unquoted value" $ do
      let result = testReadM namedValue "tautology=BVAL-is-crap"
      result `shouldBe` Right ("tautology", "BVAL-is-crap")
    it "name with unquoted empty value" $ do
      let result = testReadM namedValue "tautology="
      result `shouldBe` Right ("tautology", "")
    it "name without value" $ do
      let result = testReadM namedValue "tautology"
      isLeft result `shouldBe` True
    it "name with quoted value" $ do
      let result = testReadM namedValue "tautology=\"BVAL is crap\""
      result `shouldBe` Right ("tautology", "BVAL is crap")
    it "name with quoted empty value" $ do
      let result = testReadM namedValue "tautology=\"\""
      result `shouldBe` Right ("tautology", "")
    it "name with quoted value with escaped quotes" $ do
      let result = testReadM namedValue "tautology=\"BVAL \\\"is\\\" crap\""
      result `shouldBe` Right ("tautology", "BVAL \"is\" crap")
    it "name with quoted value missing ending quote" $ do
      let result = testReadM namedValue "tautology=\"BVAL is crap"
      isLeft result `shouldBe` True
    it "name with quoted value prematuring ending quote" $ do
      let result = testReadM namedValue "tautology=\"BVAL is\" crap"
      isLeft result `shouldBe` True
    it "incorrect named value (invalid first char)" $ do
      let result = testReadM namedValue "1tautology=\"BVAL is crap\""
      isLeft result `shouldBe` True
    it "incorrect named value (has invalid name chars)" $ do
      let result = testReadM namedValue "taut@l@gy=\"BVAL is crap\""
      isLeft result `shouldBe` True
    it "named value with no name" $ do
      let result = testReadM namedValue "=\"BVAL is crap\""
      isLeft result `shouldBe` True
