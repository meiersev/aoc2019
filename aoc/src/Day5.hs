module Day5 where

import Intcode
import Utils

inputFile = "test/resources/day5/input"

testInputReadWrite = [3,0,4,0,99]
testInputParameterModes = [1002,4,3,4,33]

testInputPart2_1 = [3,9,8,9,10,9,4,9,99,-1,8]
testInputPart2_2 = [3,9,7,9,10,9,4,9,99,-1,8]
testInputPart2_3 = [3,3,1108,-1,8,3,4,3,99]
testInputPart2_4 = [3,3,1107,-1,8,3,4,3,99]
testInputPart2_5 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
testInputPart2_6 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
testInputPart2_7 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

testInputsPart2 = [testInputPart2_1, testInputPart2_2, testInputPart2_3, testInputPart2_4, testInputPart2_5, testInputPart2_6, testInputPart2_7]

runProgPart1 :: IO()
runProgPart1 = do
    input <- readFile inputFile
    let split = splitString input ','
    let numbers = map (\s -> read s :: Int) split
    let resCtx = runWithInputs numbers [0]
    print $ outputs resCtx
    return ()

testPart2 :: IO()
testPart2 = do
    putStrLn "select test (1-7):"
    whichTest <- getLine
    let testN = read whichTest :: Int
    let input = testInputsPart2!!(testN-1)
    let resCtx = runWithInputs input [5]
    print $ outputs resCtx
    return ()
