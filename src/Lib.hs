{-# LANGUAGE QuasiQuotes #-}

module Lib where

import           Prelude hiding (null)
import           Data.Void (Void)
import           Data.Map (Map)
import qualified Data.Map as M

import           Text.RawString.QQ
import           Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Object = Map String Value

data Value = Null
           | Number Integer
           | String String
           | Boolean Bool
           | Array [Value]
           | Obj Object
           deriving (Eq, Show)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme MP.space

integer :: Parser Integer
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol MP.space

comma :: Parser String
comma = symbol ","

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = MP.between (symbol "[") (symbol "]")

identifier :: Parser String
identifier = (:) <$> MP.letterChar <*> MP.many MP.alphaNumChar

bool :: Parser Value
bool = do
  b <- MP.string "true" <|> MP.string "false"
  pure $ Boolean $
    case b of
      "true" -> True
      "false" -> False

number :: Parser Value
number = Number <$> L.signed MP.space integer

null :: Parser Value
null = MP.string "null" *> pure Null

stringLiteral :: Parser String
stringLiteral = MP.char '"' *> MP.manyTill L.charLiteral (MP.char '"')

string :: Parser Value
string = String <$> stringLiteral

array :: Parser Value
array = Array <$> brackets (value `MP.sepBy` comma)

value :: Parser Value
value = number <|> null <|> bool <|> string <|> array <|> object

row :: Parser (String, Value)
row = do
  k <- stringLiteral
  MP.space
  symbol ":"
  v <- value
  pure (k, v)

object :: Parser Value
object = Obj <$> M.fromList <$> braces (row `MP.sepBy` comma)

x :: String
x = [r|{
  "booleano": true,
  "color": "#82b92c",
  "nulo": null,
  "fodases": {
    "iai": "parsero",
    "nao": "ligo",
    "kk": 1232
  }
}
|]

y :: String
y = [r|{"booleano": true, "color": "#82b92c", "nulo": null, "fodase" : {"iai":213123, "kk": null}}|]

z :: String
z = [r|
{
  "array": [
    1,
    2,
    3
  ],
  "boolean": true,
  "color": "#82b92c",
  "null": null,
  "number": 123,
  "object": {
    "a": "b",
    "c": "d",
    "e": "f"
  },
  "string": "Hello World"
}
|]
