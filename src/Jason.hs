module Jason
  ( parseJson
  , parseJson'
  , value
  , Object
  , ParseError
  , Parser
  , Value(..)
  ) where

import           Prelude hiding (null)
import           Data.Void (Void)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Scientific (Scientific)

import           Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseError = MP.ParseErrorBundle String Void

type Object = Map String Value

data Value = Null
           | Number Scientific
           | String String
           | Boolean Bool
           | Array [Value]
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

bool :: Parser Value
bool = do
  b <- MP.string "true" <|> MP.string "false"
  pure $ Boolean $
    case b of
      "true" -> True
      "false" -> False

number :: Parser Value
number = Number <$> scientific

null :: Parser Value
null = MP.string "null" *> pure Null

stringLiteral :: Parser String
stringLiteral = MP.char '"' *> MP.manyTill L.charLiteral (MP.char '"')

string :: Parser Value
string = String <$> stringLiteral

array :: Parser Value
array = Array <$> brackets (value `MP.sepBy` comma)

row :: Parser (String, Value)
row = do
  k <- stringLiteral
  symbolic ':'
  v <- value
  pure (k, v)

object :: Parser Value
object = Obj <$> M.fromList <$> braces (row `MP.sepBy` comma)

value :: Parser Value
value = tokenize $ number <|> null <|> bool <|> string <|> array <|> object

parseJson :: String -> Either ParseError Value
parseJson = MP.parse value mempty

parseJson' :: String -> IO ()
parseJson' = MP.parseTest value
