module Day7 where

import Data.List
import Utils
import Intcode

phases :: [[Int]]
phases = permutations [0..4]

testInput :: [Int]
testInput = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 

inputFile = "test/resources/day7/input"

unflattenSignals :: [Int] -> [[Int]]
unflattenSignals [] = []
unflattenSignals (x:xs) = [x] : unflattenSignals xs

runAmplifier :: Int -> Int -> [Int] -> Int
runAmplifier phase signal prog = head $ runWithIOStub 0 prog [phase, signal]

chainAmplifiers :: [Int] -> Int -> [Int] -> Int
chainAmplifiers (p:[]) signal prog = runAmplifier p signal prog
chainAmplifiers (p:ps) signal prog = let
    firstRun = runAmplifier p signal prog
    in chainAmplifiers ps firstRun prog

tryAllPhases :: [[Int]] -> [Int] -> (Int, [Int])
tryAllPhases (p:[]) prog = (chainAmplifiers p 0 prog, p)
tryAllPhases (p:ps) prog = let 
    nextVal = chainAmplifiers p 0 prog
    other = tryAllPhases ps prog
    in if (nextVal > fst other) then (nextVal, p) else other

runPart1 :: IO ()
runPart1 = do
    input <- readFile inputFile
    let split = splitString input ','
        numbers = map (\s -> read s :: Int) split
    print $ tryAllPhases phases numbers
