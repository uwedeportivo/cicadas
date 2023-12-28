module Main (main)  where

import Test.Hspec
import EMPTriggerInternal
import EMPTrigger

someFuncSpec :: Spec
someFuncSpec = describe "someFunc" $ do
  it "three" $ do
    someFunc 3 `shouldBe` 5

someFuncImplSpec :: Spec
someFuncImplSpec = describe "someFuncImpl" $ do
  it "three" $ do
    someFuncImpl 3 `shouldBe` 5

main :: IO ()
main = hspec $ do
  someFuncSpec
  someFuncImplSpec
