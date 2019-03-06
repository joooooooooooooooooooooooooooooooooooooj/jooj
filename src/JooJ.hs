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

tokenize :: Parser a -> Parser a
tokenize p = MP.space *> p <* MP.space

scientific :: Parser Scientific
scientific = tokenize (L.signed MP.space L.scientific)

symbolic :: Char -> Parser Char
symbolic = tokenize . MP.char

comma :: Parser Char
comma = symbolic ','

braces :: Parser a -> Parser a
braces = MP.between (symbolic '{') (symbolic '}')

brackets :: Parser a -> Parser a
brackets = MP.between (symbolic '[') (symbolic ']')

identifier :: Parser String
identifier = (:) <$> MP.letterChar <*> MP.many MP.alphaNumChar

bool :: Parser JsonValue
bool = do
  b <- MP.string "true" <|> MP.string "false"
  pure $ Boolean $
    case b of
      "true" -> True
      "false" -> False

number :: Parser JsonValue
number = Number <$> scientific

null :: Parser JsonValue
null = MP.string "null" *> pure Null

stringLiteral :: Parser String
stringLiteral = MP.char '"' *> MP.manyTill L.charLiteral (MP.char '"')

string :: Parser JsonValue
string = String <$> stringLiteral

array :: Parser JsonValue
array = Array <$> brackets (value `MP.sepBy` comma)

row :: Parser (String, JsonValue)
row = do
  k <- stringLiteral
  symbolic ':'
  v <- value
  pure (k, v)

object :: Parser JsonValue
object = Obj <$> M.fromList <$> braces (row `MP.sepBy` comma)

value :: Parser JsonValue
value = tokenize $ number <|> null <|> bool <|> string <|> array <|> object

parseJson :: String -> Either ParseError JsonValue
parseJson = MP.parse (value <* MP.eof) mempty

parseJson' :: String -> IO ()
parseJson' = MP.parseTest (value <* MP.eof)

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
