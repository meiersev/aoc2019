module UtilsSpec(spec) where

import Test.Hspec
import Utils

spec :: Spec
spec = describe "Utils" $ do
    describe "splitString" $ do
        it "splits String" $ do
            splitString "1,2,3" ',' `shouldBe` ["1","2","3"]

        it "splits empty string" $ do
            splitString "" 'a' `shouldBe` []

        it "splits not containing delmiter" $ do
            splitString "1234" 'a' `shouldBe` ["1234"]

        it "splits starting with delimiter" $ do
            splitString "a1234" 'a' `shouldBe` ["", "1234"]

        it "splits with repeating delimiter" $ do
            splitString "12$$34" '$' `shouldBe` ["12", "", "34"]

    describe "rotateList" $ do
        it "rotates empty" $ do
            rotateList 12 [] `shouldBe` ([] :: [Int])
        
        it "rotates single" $ do
            rotateList 10 [1] `shouldBe` [1]

        it "rotates two" $ do
            rotateList 3 [1,2] `shouldBe` [2,1]

        it "rotates multiple" $ do
            rotateList 2 [1..5] `shouldBe` [4,5,1,2,3]

        it "rotates negative" $ do
            rotateList (-2) [1..5] `shouldBe` [3,4,5,1,2]
