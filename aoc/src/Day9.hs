module Day9 where

import Utils
import Intcode

inputFile = "test/resources/day9/input"

input :: IO [Int]
input = do
    i <- readFile inputFile
    let split = splitString i ','
    let prog = map (\s -> read s :: Int) split
    return prog

runPart1 = do
    prog <- input
    print $ outputs $ runWithInputs prog [1]
