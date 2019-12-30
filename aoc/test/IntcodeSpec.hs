module IntcodeSpec(spec) where

import Test.Hspec
import Intcode
import qualified Data.IntMap.Strict as IntMap

simpleEndState = IntMap.elems.prog.runSimple

spec :: Spec
spec = 
    describe "Intcode.run" $ do
        it "does addition (day2 example)" $
            simpleEndState [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

        it "does multiplication (day2 example)" $ 
            simpleEndState [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

        it "does multiplication after end (day2 example)" $
            simpleEndState [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]

        it "does combination (day2 example)" $
            simpleEndState [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

        it "can handle parameter modes" $
            simpleEndState [1002,4,3,4,33] `shouldBe` [1002,4,3,4,99]

        it "can compare in position mode, equal" $
            simpleEndState [8,5,6,0,99,8,8] `shouldBe` [1,5,6,0,99,8,8]

        it "can compare in position mode, not equal" $
            simpleEndState [8,5,6,0,99,7,8] `shouldBe` [0,5,6,0,99,7,8]

        it "can compare in immediate mode, equal" $
            simpleEndState [1108,8,8,5,99,-1] `shouldBe` [1108,8,8,5,99,1]

        it "can compare in immediate mode, not equal" $
            simpleEndState [1108,7,8,5,99,-1] `shouldBe` [1108,7,8,5,99,0]

        it "can compare in position mode, less than" $
            simpleEndState [7,5,6,0,99,7,8] `shouldBe` [1,5,6,0,99,7,8]

        it "can compare in position mode, not less than" $
            simpleEndState [7,5,6,0,99,8,8] `shouldBe` [0,5,6,0,99,8,8]

        it "can compare in immediate mode, less than" $
            simpleEndState [1107,7,8,5,99,-1] `shouldBe` [1107,7,8,5,99,1]

        it "can compare in immediate mode, not less than" $
            simpleEndState [1107,8,8,5,99,-1] `shouldBe` [1107,8,8,5,99,0]

        it "can run with IO stub (ex1, day5)" $
            (outputs $ runWithInputs [3,9,8,9,10,9,4,9,99,-1,8] [8]) `shouldBe` [1]

        it "can run with IO stub (ex1, day5)" $
            (outputs $ runWithInputs [3,9,8,9,10,9,4,9,99,-1,8] [9]) `shouldBe` [0]

        it "can run with IO stub (ex4, day5)" $
            (outputs $ runWithInputs [3,3,1107,-1,8,3,4,3,99] [7]) `shouldBe` [1]

        it "can run with IO stub (ex4, day5)" $
            (outputs $ runWithInputs [3,3,1107,-1,8,3,4,3,99] [8]) `shouldBe` [0]

        it "behaves the same with relative mode with offset 0" $
            simpleEndState [2208,5,6,0,99,8,8] `shouldBe` [1,5,6,0,99,8,8]

        it "can use relative mode" $
            simpleEndState [109,2,2201,-2,0,7,99,0] `shouldBe` [109,2,2201,-2,0,7,99,2310]

        it "can run quine" $ 
            let quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] in
            (outputs $ runSimple quine) `shouldBe` reverse quine

        it "can handle large numbers" $
            (outputs $ runSimple [1102,34915192,34915192,7,4,7,99,0]) `shouldBe` [1219070632396864]

        it "can handle large numbers 2" $
            (outputs $ runSimple [104,1125899906842624,99]) `shouldBe` [1125899906842624]

        it "can store input in relative mode" $
            (outputs $ runWithInputs [109,2,203,1,4,3,99] [123]) `shouldBe` [123]

        it "can do reddit example 1" $
            (outputs $ runSimple [109, -1, 4, 1, 99]) `shouldBe` [-1]

        it "can do reddit example 2" $
            (outputs $ runSimple [109, -1, 104, 1, 99]) `shouldBe` [1]

        it "can do reddit example 3" $
            (outputs $ runSimple [109, -1, 204, 1, 99]) `shouldBe` [109]

        it "can do reddit example 4" $
            (outputs $ runSimple [109, 1, 9, 2, 204, -6, 99]) `shouldBe` [204]

        it "can do reddit example 5" $
            (outputs $ runSimple [109, 1, 109, 9, 204, -6, 99]) `shouldBe` [204]

        it "can do reddit example 6" $
            (outputs $ runSimple [109, 1, 209, -1, 204, -106, 99]) `shouldBe` [204]

        it "can do reddit example 7" $
            (outputs $ runWithInputs [109, 1, 3, 3, 204, 2, 99] [444]) `shouldBe` [444]

        it "can do reddit example 8" $
            (outputs $ runWithInputs [109, 1, 203, 2, 204, 2, 99] [444]) `shouldBe` [444]
