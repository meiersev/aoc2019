module Day5 where

import Intcode

testInputReadWrite = [3,0,4,0,99]
testInputParameterModes = [1002,4,3,4,33]

testIOOps :: IO()
testIOOps = do
    endState <- runWithIO 0 (return testInputReadWrite)
    print endState
