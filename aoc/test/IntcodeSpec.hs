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

        it "can handle parameter modes" $
            run 0 [1002,4,3,4,33] `shouldBe` [1002,4,3,4,99]

        it "can compare in position mode, equal" $
            run 0 [8,5,6,0,99,8,8] `shouldBe` [1,5,6,0,99,8,8]

        it "can compare in position mode, not equal" $
            run 0 [8,5,6,0,99,7,8] `shouldBe` [0,5,6,0,99,7,8]

        it "can compare in immediate mode, equal" $
            run 0 [1108,8,8,5,99,-1] `shouldBe` [1108,8,8,5,99,1]

        it "can compare in immediate mode, not equal" $
            run 0 [1108,7,8,5,99,-1] `shouldBe` [1108,7,8,5,99,0]

        it "can compare in position mode, less than" $
            run 0 [7,5,6,0,99,7,8] `shouldBe` [1,5,6,0,99,7,8]

        it "can compare in position mode, not less than" $
            run 0 [7,5,6,0,99,8,8] `shouldBe` [0,5,6,0,99,8,8]

        it "can compare in immediate mode, less than" $
            run 0 [1107,7,8,5,99,-1] `shouldBe` [1107,7,8,5,99,1]

        it "can compare in immediate mode, not less than" $
            run 0 [1107,8,8,5,99,-1] `shouldBe` [1107,8,8,5,99,0]