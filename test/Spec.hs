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

    -- Guarantees the AST is the same before and after the unparsing
    it "can unparse and parse back" $ do
      xs <- sample' (arbitrary @Value)
      map (parse . unparseJson) xs `shouldSatisfy` all isJust
