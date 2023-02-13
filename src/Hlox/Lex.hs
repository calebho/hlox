{-# LANGUAGE OverloadedStrings #-}

module Hlox.Lex (Token (..), Number (..), tokens) where

import Control.Applicative (Alternative (some), (<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy),
    Parsec,
    choice,
    many,
    manyTill,
    try,
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
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

keyword :: Text -> Parser Text
keyword s = string s <* notFollowedBy alphaNumChar

print :: Parser Token
print = lexeme (keyword "print") $> Print

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
true = lexeme (keyword "true") $> Hlox.Lex.True

false :: Parser Token
false = lexeme (keyword "false") $> Hlox.Lex.False

nil :: Parser Token
nil = lexeme (keyword "nil") $> Nil

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
and = lexeme (keyword "and") $> And

or :: Parser Token
or = lexeme (keyword "or") $> Or

var :: Parser Token
var = lexeme (keyword "var") $> Var

identifier :: Parser Token
identifier = Identifier <$> lexeme (some (letterChar <|> char '_'))

equal :: Parser Token
equal = symbol "=" $> Equal

if_ :: Parser Token
if_ = lexeme (keyword "if") $> If

else_ :: Parser Token
else_ = lexeme (keyword "else") $> Else

while :: Parser Token
while = lexeme (keyword "while") $> While

for :: Parser Token
for = lexeme (keyword "for") $> For

leftParen :: Parser Token
leftParen = symbol "(" $> LeftParen

rightParen :: Parser Token
rightParen = symbol ")" $> RightParen

leftBrace :: Parser Token
leftBrace = symbol "{" $> LeftBrace

rightBrace :: Parser Token
rightBrace = symbol "}" $> RightBrace

fun :: Parser Token
fun = lexeme (keyword "fun") $> Fun

return :: Parser Token
return = lexeme (keyword "return") $> Return

comma :: Parser Token
comma = symbol "," $> Comma

class_ :: Parser Token
class_ = lexeme (keyword "class") $> Class

super :: Parser Token
super = lexeme (keyword "super") $> Super

this :: Parser Token
this = lexeme (keyword "this") $> This

dot :: Parser Token
dot = symbol "." $> Dot

token :: Parser Token
token =
  choice
    [ try print,
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
      dot,
      try Hlox.Lex.and,
      try Hlox.Lex.or,
      try var,
      try true,
      try false,
      try nil,
      try if_,
      try else_,
      try while,
      try for,
      try class_,
      try super,
      try this,
      leftParen,
      rightParen,
      leftBrace,
      rightBrace,
      try fun,
      try Hlox.Lex.return,
      comma,
      identifier
    ]

tokens :: Parser [Token]
tokens = spaceConsumer *> manyTill token eof
