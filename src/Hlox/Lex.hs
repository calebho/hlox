{-# LANGUAGE OverloadedStrings #-}

module Hlox.Lex (Token (..), Number (..), tokens) where

import Control.Applicative (Alternative (some), (<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, choice, many, manyTill, try)
import Text.Megaparsec.Char (char, letterChar, space1, string)
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
int = Hlox.Lex.Int <$> lexeme L.decimal

float :: Parser Number
float = Float <$> lexeme L.float

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

plus :: Parser Token
plus = symbol "+" $> Plus

minus :: Parser Token
minus = symbol "-" $> Minus

star :: Parser Token
star = symbol "*" $> Star

slash :: Parser Token
slash = symbol "/" $> Slash

less :: Parser Token
less = symbol "<" $> Less

lessEqual :: Parser Token
lessEqual = symbol "<=" $> LessEqual

greater :: Parser Token
greater = symbol ">" $> Greater

greaterEqual :: Parser Token
greaterEqual = symbol ">=" $> GreaterEqual

equalEqual :: Parser Token
equalEqual = symbol "==" $> EqualEqual

bangEqual :: Parser Token
bangEqual = symbol "!=" $> BangEqual

bang :: Parser Token
bang = symbol "!" $> Bang

and :: Parser Token
and = lexeme (string "and") $> And

or :: Parser Token
or = lexeme (string "or") $> Or

var :: Parser Token
var = lexeme (string "var") $> Var

identifier :: Parser Token
identifier = Identifier <$> lexeme (some (letterChar <|> char '_'))

equal :: Parser Token
equal = symbol "=" $> Equal

token :: Parser Token
token =
  choice
    [ print,
      stringLiteral,
      numberLiteral,
      semicolon,
      plus,
      minus,
      star,
      slash,
      lessEqual,
      less,
      greaterEqual,
      greater,
      equalEqual,
      equal,
      bangEqual,
      bang,
      Hlox.Lex.and,
      Hlox.Lex.or,
      var,
      true,
      false,
      nil,
      identifier
    ]

tokens :: Parser [Token]
tokens = spaceConsumer *> manyTill token eof
