{-# LANGUAGE TemplateHaskell #-}

module Hlox.LexSpec (spec) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import qualified Data.Text as T
import Data.Void (Void)
import Hlox.Lex (Number (..), Token (..), tokens)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

newtype ParseResult = ParseResult (Either (ParseErrorBundle T.Text Void) [Token]) deriving (Eq)

instance Show ParseResult where
  show (ParseResult (Right ts)) = show ts
  show (ParseResult (Left err)) = errorBundlePretty err

spec :: Spec
spec =
  describe "Lex" $ do
    it "lexes hello.lox" $ do
      let src = $(makeRelativeToProject "test/data/hello.lox" >>= embedStringFile) :: T.Text
      let expected = [Print, String "Hello, world!", Semicolon]

      let result = runParser tokens "" src

      result `shouldBe` Right expected

    it "lexes literals.lox" $ do
      let src = $(makeRelativeToProject "test/data/literals.lox" >>= embedStringFile) :: T.Text
      let expected =
            [ Hlox.Lex.True,
              Semicolon,
              Hlox.Lex.False,
              Semicolon,
              Number (Hlox.Lex.Int 1234),
              Semicolon,
              Number (Hlox.Lex.Int (-5678)),
              Semicolon,
              Number (Float 12.34),
              Semicolon,
              Number (Float (-56.78)),
              Semicolon,
              String "I am a string",
              Semicolon,
              String "",
              Semicolon,
              String "123",
              Semicolon,
              Nil,
              Semicolon
            ]

      let result = ParseResult $ runParser tokens "" src

      result `shouldBe` ParseResult (Right expected)
