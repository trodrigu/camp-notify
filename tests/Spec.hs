module Spec where

import Test.Tasty
import Test.Tasty.Hspec

scrapeStateParksTest :: Spec
scrapeStateParksTest = do
  describe "scrapeStateParks" $ do
    it "returns state parks" $ scrapeStateParks `shouldBe` "Great Basin"


scrapeParksTestGroups :: TestTree
scrapeParksTestGroups = testGroup "ScrapeParksTestGroup" [scrapeStateParksTest]

main :: IO ()
main = 
  tests <- testSpec "TestSuite" scrapeParksTestGroups
  defaultMain tests
