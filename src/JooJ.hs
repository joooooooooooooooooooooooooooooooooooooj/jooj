module JooJ
  ( parseJson
  , parseJson'
  , unparseJson
  , value
  , Object
  , ParseError
  , Parser
  , JsonValue(..)
  ) where

import           Prelude hiding (null)
import           Control.Applicative (empty)
import           Control.Monad (void)

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Scientific (Scientific)
import           Data.Void (Void)

import           Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseError = MP.ParseErrorBundle String Void

type Object = Map String JsonValue

data JsonValue = Null
           | Number Scientific
           | String String
           | Boolean Bool
           | Array [JsonValue]
           | Obj Object
           deriving (Eq, Show)

sc :: Parser ()
sc = L.space MP.space1 (void $ MP.oneOf [' ', '\t']) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = MP.between (symbol "[") (symbol "]")

identifier :: Parser String
identifier = lexeme $
  (:) <$> MP.letterChar <*> MP.many MP.alphaNumChar

bool :: Parser JsonValue
bool = do
  b <- MP.string "true" <|> MP.string "false"
  pure $ Boolean $
    case b of
      "true" -> True
      "false" -> False

number :: Parser JsonValue
number = Number <$> L.signed sc (lexeme L.scientific)

null :: Parser JsonValue
null = MP.string "null" *> pure Null

stringLiteral :: Parser String
stringLiteral = lexeme $
  MP.char '"' *> MP.manyTill L.charLiteral (MP.char '"')

string :: Parser JsonValue
string = String <$> stringLiteral

array :: Parser JsonValue
array = Array <$> brackets (value `MP.sepBy` comma)

row :: Parser (String, JsonValue)
row = (,) <$> (stringLiteral <* colon) <*> value

object :: Parser JsonValue
object = Obj <$> M.fromList <$> braces (row `MP.sepBy` comma)

value :: Parser JsonValue
value = lexeme $ MP.choice
  [ number
  , null
  , bool
  , string
  , array
  , object
  ]

parseJson :: String -> Either ParseError JsonValue
parseJson = MP.parse (MP.space *> value <* MP.eof) mempty

parseJson' :: String -> IO ()
parseJson' = MP.parseTest (MP.space *> value <* MP.eof)

unparseJson :: JsonValue -> String
unparseJson val =
  case val of
    Null -> "null"
    Number n -> show n
    Boolean True -> "true"
    Boolean False -> "false"
    String str -> show str
    Array arr -> show $ map unparseJson arr
    Obj obj -> braces $ intercalate ", " $
      M.foldrWithKey f [] obj where

        f :: String -> JsonValue -> [String] -> [String]
        f k v acc = (show k ++ ": " ++ unparseJson v) : acc

        braces :: String -> String
        braces str = "{" ++ str ++ "}"
