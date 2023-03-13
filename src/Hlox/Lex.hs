{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hlox.Lex (Token (..), Number (..), tokens, WithPos (..)) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, getParserState, notFollowedBy),
    Parsec,
    PosState (pstateSourcePos),
    SourcePos (sourceColumn),
    State (statePosState),
    choice,
    many,
    manyTill,
    try,
    unPos,
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (print)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser (WithPos Text)
symbol s =
  withPos $ \startPos -> do
    L.symbol spaceConsumer s
    return WithPos {startPos, tokenLength = Txt.length s, tokenVal = s}

data Number
  = Int Int
  | Float Float
  deriving (Eq, Ord)

instance Show Number where
  show = \case
    Int n -> show n
    Float f -> show f

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
  deriving (Eq, Ord)

instance Show Token where
  show = \case
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    Comma -> ","
    Dot -> "."
    Minus -> "-"
    Plus -> "+"
    Semicolon -> ";"
    Slash -> "/"
    Star -> "*"
    Bang -> "!"
    BangEqual -> "!="
    Equal -> "="
    EqualEqual -> "=="
    Greater -> ">"
    GreaterEqual -> ">="
    Less -> "<"
    LessEqual -> "<="
    Identifier s -> show s
    String s -> [i|"#{s}"|]
    Number n -> show n
    And -> "and"
    Class -> "class"
    Else -> "else"
    Hlox.Lex.False -> "false"
    Fun -> "fun"
    For -> "for"
    If -> "if"
    Nil -> "nil"
    Or -> "or"
    Print -> "print"
    Return -> "return"
    Super -> "super"
    This -> "this"
    Hlox.Lex.True -> "true"
    Var -> "var"
    While -> "while"

data WithPos a = WithPos
  { startPos :: SourcePos,
    -- endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Show, Ord)

instance Functor WithPos where
  fmap f x@WithPos {tokenVal} = x {tokenVal = f tokenVal}

getSourcePos :: MonadParsec e s m => m SourcePos
getSourcePos = pstateSourcePos . statePosState <$> getParserState

withPos :: MonadParsec e s m => (SourcePos -> m a) -> m a
withPos f = getSourcePos >>= f

getCol :: SourcePos -> Int
getCol = unPos . sourceColumn

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_'

keyword :: Text -> Parser (WithPos Text)
keyword k =
  withPos $ \startPos -> do
    string k <* notFollowedBy identifierChar
    return WithPos {startPos, tokenLength = Txt.length k, tokenVal = k}

print :: Parser (WithPos Token)
print = ($> Print) <$> lexeme (keyword "print")

stringLiteral :: Parser (WithPos Token)
stringLiteral =
  withPos $ \startPos -> do
    s <- lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
    return WithPos {startPos, tokenLength = length s, tokenVal = String s}

int :: Parser (WithPos Number)
int =
  withPos $ \startPos -> do
    n <- lexeme L.decimal <* notFollowedBy identifierChar
    endPos <- getSourcePos
    return
      WithPos
        { startPos,
          tokenLength = getCol endPos - getCol startPos,
          tokenVal = Hlox.Lex.Int n
        }

float :: Parser (WithPos Number)
float =
  withPos $ \startPos -> do
    x <- lexeme L.float <* notFollowedBy identifierChar
    endPos <- getSourcePos
    return
      WithPos
        { startPos,
          tokenLength = getCol endPos - getCol startPos,
          tokenVal = Float x
        }

numberLiteral :: Parser (WithPos Token)
numberLiteral = do
  n <- try float <|> int
  return (Number <$> n)

semicolon :: Parser (WithPos Token)
semicolon = fmap ($> Semicolon) (symbol ";")

true :: Parser (WithPos Token)
true = ($> Hlox.Lex.True) <$> lexeme (keyword "true")

false :: Parser (WithPos Token)
false = ($> Hlox.Lex.False) <$> lexeme (keyword "false")

nil :: Parser (WithPos Token)
nil = ($> Nil) <$> lexeme (keyword "nil")

plus :: Parser (WithPos Token)
plus = ($> Plus) <$> symbol "+"

minus :: Parser (WithPos Token)
minus = ($> Minus) <$> symbol "-"

star :: Parser (WithPos Token)
star = ($> Star) <$> symbol "*"

slash :: Parser (WithPos Token)
slash = ($> Slash) <$> symbol "/"

less :: Parser (WithPos Token)
less = ($> Less) <$> symbol "<"

lessEqual :: Parser (WithPos Token)
lessEqual = ($> LessEqual) <$> symbol "<="

greater :: Parser (WithPos Token)
greater = ($> Greater) <$> symbol ">"

greaterEqual :: Parser (WithPos Token)
greaterEqual = ($> GreaterEqual) <$> symbol ">="

equalEqual :: Parser (WithPos Token)
equalEqual = ($> EqualEqual) <$> symbol "=="

bangEqual :: Parser (WithPos Token)
bangEqual = ($> BangEqual) <$> symbol "!="

bang :: Parser (WithPos Token)
bang = ($> Bang) <$> symbol "!"

and :: Parser (WithPos Token)
and = ($> And) <$> lexeme (keyword "and")

or :: Parser (WithPos Token)
or = ($> Or) <$> lexeme (keyword "or")

var :: Parser (WithPos Token)
var = ($> Var) <$> lexeme (keyword "var")

identifier :: Parser (WithPos Token)
identifier =
  withPos $ \startPos -> do
    id <- lexeme ((:) <$> (letterChar <|> char '_') <*> many identifierChar)
    endPos <- getSourcePos
    return
      WithPos
        { startPos,
          tokenLength = getCol endPos - getCol startPos,
          tokenVal = Identifier id
        }

equal :: Parser (WithPos Token)
equal = ($> Equal) <$> symbol "="

if_ :: Parser (WithPos Token)
if_ = ($> If) <$> lexeme (keyword "if")

else_ :: Parser (WithPos Token)
else_ = ($> Else) <$> lexeme (keyword "else")

while :: Parser (WithPos Token)
while = ($> While) <$> lexeme (keyword "while")

for :: Parser (WithPos Token)
for = ($> For) <$> lexeme (keyword "for")

leftParen :: Parser (WithPos Token)
leftParen = ($> LeftParen) <$> symbol "("

rightParen :: Parser (WithPos Token)
rightParen = ($> RightParen) <$> symbol ")"

leftBrace :: Parser (WithPos Token)
leftBrace = ($> LeftBrace) <$> symbol "{"

rightBrace :: Parser (WithPos Token)
rightBrace = ($> RightBrace) <$> symbol "}"

fun :: Parser (WithPos Token)
fun = ($> Fun) <$> lexeme (keyword "fun")

return_ :: Parser (WithPos Token)
return_ = ($> Return) <$> lexeme (keyword "return")

comma :: Parser (WithPos Token)
comma = ($> Comma) <$> symbol ","

class_ :: Parser (WithPos Token)
class_ = ($> Class) <$> lexeme (keyword "class")

super :: Parser (WithPos Token)
super = ($> Super) <$> lexeme (keyword "super")

this :: Parser (WithPos Token)
this = ($> This) <$> lexeme (keyword "this")

dot :: Parser (WithPos Token)
dot = ($> Dot) <$> symbol "."

token :: Parser (WithPos Token)
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
      try return_,
      comma,
      identifier
    ]

tokens :: Parser [WithPos Token]
tokens = spaceConsumer *> manyTill token eof
