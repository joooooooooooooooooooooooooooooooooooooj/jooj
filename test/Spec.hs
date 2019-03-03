{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

import Data.Maybe (isJust)
import Data.Scientific
import JooJ
import System.Directory (listDirectory)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Scientific
import Text.RawString.QQ

instance Arbitrary Value where
  arbitrary = oneof
    [ pure Null
    , Number <$> arbitrary
    , String <$> arbitrary
    , Boolean <$> arbitrary
    , Array <$> arbitrary
    , Obj <$> arbitrary
    ]

parse :: String -> Maybe Value
parse = either (const Nothing) Just . parseJson

parseFile :: FilePath -> IO (Maybe Value)
parseFile path = parse <$> readFile path

parseFiles :: FilePath -> IO [Maybe Value]
parseFiles path = listDirectory path >>= traverse (parseFile . (path++))

main :: IO ()
main = hspec $ do
  describe "Parse JSON" $ do
    it "parse test files" $ do
      res <- parseFiles "test/files/"
      res `shouldSatisfy` all isJust

    it "can parse formatted" $ do
      parse x `shouldSatisfy` isJust

    it "can parse inline string" $ do
      parse y `shouldSatisfy` isJust

    it "can parse object inside object" $ do
      parse z `shouldSatisfy` isJust

    it "can parse scientific notation" $ do
      lst <- sample' (arbitrary @Scientific)
      parse . show <$> lst `shouldSatisfy` all isJust

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
  "string": "Hello World",

  "oq":{
    "0": {
      "neurons": "300",
      "weights": [1,2,3,4,5],
      "scisci": {
        "one" :2.5e-3,
        "twooo":   -1.3e-3
      }
    }
  }
  }
|]
