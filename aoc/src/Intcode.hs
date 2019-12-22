module Intcode(run) where 

import Utils

valAt :: Int -> [Int] -> Int
valAt pos state = state!!(state!!pos)

doOp :: Int -> [Int] -> (Int -> Int -> Int) -> Int
doOp pos state biFunc = (valAt (pos+1) state) `biFunc` (valAt (pos+2) state)

doAdd :: Int -> [Int] -> Int
doAdd pos state = doOp pos state (+)

doMul :: Int -> [Int] -> Int
doMul pos state = doOp pos state (*)

runStep :: Int -> [Int] -> (Int -> [Int] -> Int) -> [Int]
runStep pos state op = replace (state!!(pos+3)) (op pos state) state

run :: Int -> [Int] -> [Int]
run pos state
    | state!!pos == 99 = state
    | state!!pos == 1 = run (pos + 4) (runStep pos state doAdd)
    | state!!pos == 2 = run (pos + 4) (runStep pos state doMul)
    | otherwise = [-1]
