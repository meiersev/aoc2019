
module UtilsSpec(spec) where

import Test.Hspec
import Utils

spec :: Spec
spec = 
    describe "splitString" $ do
        it "splits String" $
            splitString "1,2,3" ',' `shouldBe` ["1","2","3"]

        it "splits empty string" $
            splitString "" 'a' `shouldBe` []

        it "splits not containing delmiter" $
            splitString "1234" 'a' `shouldBe` ["1234"]

        it "splits starting with delimiter" $
            splitString "a1234" 'a' `shouldBe` ["", "1234"]

        it "splits with repeating delimiter" $
            splitString "12$$34" '$' `shouldBe` ["12", "", "34"]
            