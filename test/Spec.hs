{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           GHC.Generics (Generic)
import           Test.Hspec
import qualified Z.Data.Text  as T
import qualified Z.Data.YAML  as YAML


main :: IO ()
main = hspec $ smokeSpec

data Person = Person
  { name :: T.Text
  , age  :: Int
  } deriving (Show, Generic, YAML.JSON)

smokeSpec :: Spec
smokeSpec = context "SmokeTest" $ do
  it "Z.Data.YAML.readYAMLFile" $ do
    persons <- YAML.readYAMLFile @[Person] "test/datas/smoke.yaml"
    let first = persons !! 0
    name first `shouldBe` "Alice"
    age  first `shouldBe` 16
    let second = persons !! 1
    name second `shouldBe` "Bob"
    age  second `shouldBe` 14
