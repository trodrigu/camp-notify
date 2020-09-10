module Spec where

import Test.Tasty
import Test.Tasty.Hspec

myTest :: Spec
myTest = do

  it "hello" $
    False `shouldBe` True

myTestGroup :: TestTree
myTestGroup = testGroup "myTestGroup" [myTest]

main :: IO ()
main = 
  tests <- testSpec "test-1" myTestGroup
  defaultMain tests
