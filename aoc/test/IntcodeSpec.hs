module IntcodeSpec(spec) where

import Test.Hspec
import Intcode

spec :: Spec
spec = 
    describe "Intcode.run" $ do
        it "does addition (day2 example)" $
            run 0 [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

        it "does multiplication (day2 example)" $ 
            run 0 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

        it "does multiplication after end (day2 example)" $
            run 0 [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]

        it "does combination (day2 example)" $
            run 0 [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
