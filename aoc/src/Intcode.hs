module Intcode(run, runWithIO) where 

import Utils

valAt :: Int -> [Int] -> Int
valAt pos state = state!!(state!!pos)

doOp :: Int -> [Int] -> (Int -> Int -> Int) -> Int
doOp pos state biFunc = (valAt (pos+1) state) `biFunc` (valAt (pos+2) state)

doAdd :: Int -> [Int] -> Int
doAdd pos state = doOp pos state (+)

doMul :: Int -> [Int] -> Int
doMul pos state = doOp pos state (*)

doBiFunc :: Int -> [Int] -> (Int -> [Int] -> Int) -> [Int]
doBiFunc pos state op = replace (state!!(pos+3)) (op pos state) state

inputInstr :: Int -> [Int] -> IO [Int]
inputInstr pos state = do
    input <- getLine
    let i = read input :: Int
    return (replace (state!!(pos+1)) i state)

outputInstr :: Int -> [Int] -> IO [Int]
outputInstr pos state = do
    print (valAt (pos+1) state)
    return state

runOp :: Int -> Int -> [Int] -> (Int, [Int])
runOp _ 99 state = (-1, state) 
runOp pos 1 state = (pos+4, doBiFunc pos state doAdd) 
runOp pos 2 state = (pos+4, doBiFunc pos state doMul) 
runOp _ x _ = error ("unsupported operation " ++ (show x))

runOpWithoutIO :: Int -> Int -> [Int] -> (Int, IO [Int])
runOpWithoutIO pos op state = let (newPos, newState) = runOp pos op state in (newPos, return newState)

runOpWithIO :: Int -> Int -> [Int] -> (Int, IO [Int])
runOpWithIO pos 99 state = runOpWithoutIO pos 99 state
runOpWithIO pos 1 state = runOpWithoutIO pos 1 state
runOpWithIO pos 2 state = runOpWithoutIO pos 2 state
runOpWithIO pos 3 state = (pos+2, inputInstr pos state)
runOpWithIO pos 4 state = (pos+2, outputInstr pos state)
runOpWithIO _ x _ = error ("unsupported operation " ++ (show x))

runWithIO :: Int -> IO [Int] -> IO [Int]
runWithIO pos state = do
    s <- state
    let op = s!!pos
        (nextPos, nextState) = runOpWithIO pos op s
    if (nextPos < 0) then nextState
    else runWithIO nextPos nextState

run :: Int -> [Int] -> [Int]
run pos state = let (nextPos, nextState) = runOp pos (state!!pos) state in
    if (nextPos < 0) then nextState
    else run nextPos nextState
