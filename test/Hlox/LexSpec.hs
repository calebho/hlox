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
              Number (Float 12.34),
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

    it "lexes operators.lox" $ do
      let src = $(makeRelativeToProject "test/data/operators.lox" >>= embedStringFile) :: T.Text
      let expected =
            [ -- 1. addition
              Number (Hlox.Lex.Int 1),
              Plus,
              Number (Hlox.Lex.Int 2),
              Semicolon,
              Number (Float 1.0),
              Plus,
              Number (Hlox.Lex.Int 2),
              Semicolon,
              Number (Hlox.Lex.Int 1),
              Plus,
              Number (Float 2.0),
              Semicolon,
              Number (Float 1),
              Plus,
              Number (Float 2),
              Semicolon,
              -- 2. subtraction
              Number (Hlox.Lex.Int 10),
              Minus,
              Number (Hlox.Lex.Int 4),
              Semicolon,
              Number (Float 10.0),
              Minus,
              Number (Hlox.Lex.Int 4),
              Semicolon,
              Number (Hlox.Lex.Int 10),
              Minus,
              Number (Float 4.0),
              Semicolon,
              Number (Float 10.0),
              Minus,
              Number (Float 4.0),
              Semicolon,
              -- 3. negation
              Minus,
              Number (Hlox.Lex.Int 10),
              Semicolon,
              Minus,
              Number (Float 10.0),
              Semicolon,
              -- 4. multiplication
              Number (Hlox.Lex.Int 50),
              Star,
              Number (Hlox.Lex.Int 7),
              Semicolon,
              Number (Hlox.Lex.Int 50),
              Star,
              Number (Float 7.0),
              Semicolon,
              Number (Float 50.0),
              Star,
              Number (Hlox.Lex.Int 7),
              Semicolon,
              Number (Float 50.0),
              Star,
              Number (Float 7.0),
              Semicolon,
              -- 5. division
              Number (Hlox.Lex.Int 3),
              Slash,
              Number (Hlox.Lex.Int 6),
              Semicolon,
              Number (Hlox.Lex.Int 3),
              Slash,
              Number (Float 6.0),
              Semicolon,
              Number (Float 3.0),
              Slash,
              Number (Hlox.Lex.Int 6),
              Semicolon,
              Number (Float 3.0),
              Slash,
              Number (Float 6.0),
              Semicolon,
              -- 6. less than
              Number (Hlox.Lex.Int 42),
              Less,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              Less,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              Less,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              Less,
              Number (Float 3.0),
              Semicolon,
              -- 7. less equal
              Number (Hlox.Lex.Int 42),
              LessEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              LessEqual,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              LessEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              LessEqual,
              Number (Float 3.0),
              Semicolon,
              -- 8. greater
              Number (Hlox.Lex.Int 42),
              Greater,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              Greater,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              Greater,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              Greater,
              Number (Float 3.0),
              Semicolon,
              -- 9. greater equal
              Number (Hlox.Lex.Int 42),
              GreaterEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              GreaterEqual,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              GreaterEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              GreaterEqual,
              Number (Float 3.0),
              Semicolon,
              -- 10. equal equal
              Number (Hlox.Lex.Int 42),
              EqualEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              EqualEqual,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              EqualEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              EqualEqual,
              Number (Float 3.0),
              Semicolon,
              -- 11. bang equal
              Number (Hlox.Lex.Int 42),
              BangEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Hlox.Lex.Int 42),
              BangEqual,
              Number (Float 3.0),
              Semicolon,
              Number (Float 42.0),
              BangEqual,
              Number (Hlox.Lex.Int 3),
              Semicolon,
              Number (Float 42.0),
              BangEqual,
              Number (Float 3.0),
              Semicolon,
              -- 12. bang
              Bang,
              Hlox.Lex.True,
              Semicolon,
              Bang,
              Hlox.Lex.False,
              Semicolon,
              -- 13. and
              Hlox.Lex.True,
              And,
              Hlox.Lex.False,
              Semicolon,
              Hlox.Lex.False,
              And,
              Hlox.Lex.True,
              Semicolon,
              -- 14. or
              Hlox.Lex.True,
              Or,
              Hlox.Lex.False,
              Semicolon,
              Hlox.Lex.False,
              Or,
              Hlox.Lex.True,
              Semicolon
            ]

      let result = ParseResult $ runParser tokens "" src

      result `shouldBe` ParseResult (Right expected)

    it "lexes variables.lox" $ do
      let src = $(makeRelativeToProject "test/data/variables.lox" >>= embedStringFile) :: T.Text
      let expected =
            [ Var,
              Identifier "imAVariable",
              Equal,
              String "here is my value",
              Semicolon,
              Var,
              Identifier "im_another_variable",
              Equal,
              String "here is my value",
              Semicolon,
              Var,
              Identifier "iAmNil",
              Semicolon,
              --
              Var,
              Identifier "breakfast",
              Equal,
              String "bagels",
              Semicolon,
              Print,
              Identifier "breakfast",
              Semicolon,
              Identifier "breakfast",
              Equal,
              String "beignets",
              Semicolon,
              Print,
              Identifier "breakfast",
              Semicolon,
              --
              Var,
              Identifier "andVar",
              Semicolon,
              Var,
              Identifier "classVar",
              Semicolon,
              Var,
              Identifier "elseVar",
              Semicolon,
              Var,
              Identifier "falseVar",
              Semicolon,
              Var,
              Identifier "funVar",
              Semicolon,
              Var,
              Identifier "forVar",
              Semicolon,
              Var,
              Identifier "ifVar",
              Semicolon,
              Var,
              Identifier "nilVar",
              Semicolon,
              Var,
              Identifier "orVar",
              Semicolon,
              Var,
              Identifier "printVar",
              Semicolon,
              Var,
              Identifier "returnVar",
              Semicolon,
              Var,
              Identifier "superVar",
              Semicolon,
              Var,
              Identifier "thisVar",
              Semicolon,
              Var,
              Identifier "trueVar",
              Semicolon,
              Var,
              Identifier "varVar",
              Semicolon,
              Var,
              Identifier "whileVar",
              Semicolon
            ]

      let result = ParseResult $ runParser tokens "" src

      result `shouldBe` ParseResult (Right expected)

    it "lexes control_flow.lox" $ do
      let src = $(makeRelativeToProject "test/data/control_flow.lox" >>= embedStringFile) :: T.Text
      let expected =
            [ If,
              LeftParen,
              Identifier "condition",
              RightParen,
              LeftBrace,
              Print,
              String "yes",
              Semicolon,
              RightBrace,
              Else,
              LeftBrace,
              Print,
              String "no",
              Semicolon,
              RightBrace,
              --
              Var,
              Identifier "a",
              Equal,
              Number (Hlox.Lex.Int 1),
              Semicolon,
              While,
              LeftParen,
              Identifier "a",
              Less,
              Number (Hlox.Lex.Int 10),
              RightParen,
              LeftBrace,
              Print,
              Identifier "a",
              Semicolon,
              Identifier "a",
              Equal,
              Identifier "a",
              Plus,
              Number (Hlox.Lex.Int 1),
              Semicolon,
              RightBrace,
              --
              For,
              LeftParen,
              Var,
              Identifier "a",
              Equal,
              Number (Hlox.Lex.Int 1),
              Semicolon,
              Identifier "a",
              Less,
              Number (Hlox.Lex.Int 10),
              Semicolon,
              Identifier "a",
              Equal,
              Identifier "a",
              Plus,
              Number (Hlox.Lex.Int 1),
              RightParen,
              LeftBrace,
              Print,
              Identifier "a",
              Semicolon,
              RightBrace
            ]

      let result = ParseResult $ runParser tokens "" src

      result `shouldBe` ParseResult (Right expected)
