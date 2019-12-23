module Day5 where

import Intcode
import Utils

inputFile = "test/resources/day5/input"

testInputReadWrite = [3,0,4,0,99]
testInputParameterModes = [1002,4,3,4,33]

testIOOps :: IO()
testIOOps = do
    endState <- runWithIO 0 (return testInputReadWrite)
    print endState

runProgPart1 :: IO()
runProgPart1 = do
    input <- readFile inputFile
    let split = splitString input ','
        numbers = return (map (\s -> read s :: Int) split)
    runWithIO 0 numbers
    return ()
