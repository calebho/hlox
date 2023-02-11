{-# LANGUAGE OverloadedStrings #-}

module Hlox.Lex (Token (..), Number (..), tokens) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, choice, many, manyTill, try)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (print)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

data Number
  = Int Int
  | Float Float
  deriving (Show, Eq)

data Token
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier String
  | String String
  | Number Number
  | -- Keywords
    And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  deriving (Show, Eq)

print :: Parser Token
print = lexeme (string "print") $> Print

stringLiteral :: Parser Token
stringLiteral = String <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

signed :: (Num a) => Parser a -> Parser a
signed = L.signed spaceConsumer

int :: Parser Number
int = Hlox.Lex.Int <$> signed (lexeme L.decimal)

float :: Parser Number
float = Float <$> signed (lexeme L.float)

numberLiteral :: Parser Token
numberLiteral = Number <$> (try float <|> int)

semicolon :: Parser Token
semicolon = symbol ";" $> Semicolon

true :: Parser Token
true = lexeme (string "true") $> Hlox.Lex.True

false :: Parser Token
false = lexeme (string "false") $> Hlox.Lex.False

nil :: Parser Token
nil = lexeme (string "nil") $> Nil

token :: Parser Token
token =
  choice
    [ print,
      stringLiteral,
      numberLiteral,
      semicolon,
      true,
      false,
      nil
    ]

tokens :: Parser [Token]
tokens = spaceConsumer *> manyTill token eof
