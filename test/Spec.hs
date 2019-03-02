{-# LANGUAGE QuasiQuotes #-}

import Data.Maybe (isJust)
import Jason (value, Value(..))
import Test.Hspec
import Text.RawString.QQ
import Text.Megaparsec (parseMaybe)

parse :: String -> Maybe Value
parse = parseMaybe value

main :: IO ()
main = hspec $ do
  describe "Parse JSON" $ do
    it "can parse formatted" $ do
      parse x `shouldSatisfy` isJust

    it "can parse inline string" $ do
      parse y `shouldSatisfy` isJust

    it "can parse object inside object" $ do
      parse z `shouldSatisfy` isJust

    it "can parse douglas" $ do
      parse w `shouldSatisfy` isJust

    it "can parse scientific notation" $ do
      parse "-1.0e+3" `shouldSatisfy` isJust

x :: String
x = [r|{
  "booleano": true,
  "color" :"#82b92c",
  "nulo": null,
  "fodases": {
    "iai": "parsero",
    "nao": "ligo",
    "kk": -1232
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
  "another": 12345.055,
  "object": {
    "a": "b",
    "c": "d",
    "e": "f"
  },
  "string": "Hello World"
}
|]

w :: String
w = [r|{
  "0": {
    "neurons": "300",
    "weights": [1,2,3,4,5]
    "scisci": {
      "one" :2.5e-3,
      "twooo":   -1.3e-3
    }
  }
}|]
