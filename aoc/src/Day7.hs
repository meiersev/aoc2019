module Day7 where

import Data.List
import Utils
import Intcode

phases :: [[Int]]
phases = permutations [0..4]

testInput :: [Int]
testInput = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 

inputFile = "test/resources/day7/input"

runAmplifier :: Int -> Int -> Program -> Int
runAmplifier phase signal prog = head $ outputs resCtx
  where 
    resCtx = runWithInputs prog [phase, signal]

chainAmplifiers :: [Int] -> Int -> Program -> Int
chainAmplifiers (p:[]) signal prog = runAmplifier p signal prog
chainAmplifiers (p:ps) signal prog = let
    firstRun = runAmplifier p signal prog
    in chainAmplifiers ps firstRun prog

tryAllPhases :: [[Int]] -> Program -> (Int, [Int])
tryAllPhases (p:[]) prog = (chainAmplifiers p 0 prog, p)
tryAllPhases (p:ps) prog = let 
    nextVal = chainAmplifiers p 0 prog
    other = tryAllPhases ps prog
    in if nextVal > fst other then (nextVal, p) else other

runPart1 :: IO ()
runPart1 = do
    input <- readFile inputFile
    let split = splitString input ','
        numbers = map (\s -> read s :: Int) split
    print $ tryAllPhases phases numbers

-- Part 2
phasesPt2 :: [[Int]]
phasesPt2 = permutations [5..9]

initAmplifiers :: Program -> [Program]
initAmplifiers prog = replicate 5 prog
