{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}

import Data.Maybe (fromJust, isJust)
import Data.Scientific
import JooJ
import System.Directory (listDirectory)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Scientific
import Text.RawString.QQ

instance Arbitrary JsonValue where
  arbitrary = oneof
    [ pure Null
    , Number <$> arbitrary
    , String <$> arbitrary
    , Boolean <$> arbitrary
    , Array <$> arbitrary
    , Obj <$> arbitrary
    ]

parse :: String -> Maybe JsonValue
parse = either (const Nothing) Just . parseJson

parseFile :: FilePath -> IO (Maybe JsonValue)
parseFile path = parse <$> readFile path

parseFiles :: FilePath -> IO [Maybe JsonValue]
parseFiles path = listDirectory path >>= traverse (parseFile . (path++))

main :: IO ()
main = hspec $ do
  describe "JooJ" $ do
    it "can parse the test files" $ do
      res <- parseFiles "test/files/"
      res `shouldSatisfy` all isJust

    it "can parse scientific notation" $ do
      xs <- sample' (arbitrary @Scientific)
      map (parse . show) xs `shouldSatisfy` all isJust

    it "can parse strings" $ do
      xs <- sample' (arbitrary @String)
      map (parse . show) xs `shouldSatisfy` all isJust

    it "can unparse and parse back" $ do
      res <- parseFiles "test/files/"
      map (parse . unparseJson . fromJust) res `shouldSatisfy` all isJust
