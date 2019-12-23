module Intcode(run, runWithIO) where 

import Utils

data ParameterMode = Position | Immediate deriving (Eq, Show)
toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode x = error ("unsupported parameter mode " ++ (show x))

data Operation = Operation {opCode :: Int, parameterModes :: [ParameterMode]} deriving Show
parseOperation :: Int -> Operation
parseOperation x = let 
    digits = show x
    (modeDigits, opCodeDigits) = splitAt (length digits - 2) digits
    opCode = read opCodeDigits
    modes = map (toMode.read.(\c -> [c])) (reverse modeDigits) 
    in Operation opCode modes

padParameterModes :: Int -> [ParameterMode] -> [ParameterMode]
padParameterModes n modes = let
    paddingSize = n - (length modes)
    in modes ++ (replicate paddingSize Position)

valAt :: Int -> [Int] -> Int
valAt pos state = state!!(state!!pos)

valAtImmediate :: Int -> [Int] -> Int
valAtImmediate pos state = state!!pos

doOp :: Int -> [Int] -> [ParameterMode] -> (Int -> Int -> Int) -> Int
doOp pos state modes biFunc = let
    paddedModes = padParameterModes 2 modes
    functions = map (\mode -> if (mode == Position) then valAt else valAtImmediate) paddedModes
    in ((head functions) (pos+1) state) `biFunc` ((last functions) (pos+2) state)

doAdd :: Int -> [Int] -> [ParameterMode] -> Int
doAdd pos state modes = doOp pos state modes (+)

doMul :: Int -> [Int] -> [ParameterMode] -> Int
doMul pos state modes = doOp pos state modes (*)

doBiFunc :: Int -> [Int] -> [ParameterMode] -> (Int -> [Int] -> [ParameterMode] -> Int) -> [Int]
doBiFunc pos state modes op = replace (state!!(pos+3)) (op pos state modes) state

inputInstr :: Int -> [Int] -> IO [Int]
inputInstr pos state = do
    input <- getLine
    let i = read input :: Int
    return (replace (state!!(pos+1)) i state)

outputInstr :: Int -> [Int] -> IO [Int]
outputInstr pos state = do
    print (valAt (pos+1) state)
    return state

runOp :: Int -> Operation -> [Int] -> (Int, [Int])
runOp pos op state
    | opCode op == 99 = (-1, state)
    | opCode op == 1 = (pos+4, doBiFunc pos state (parameterModes op) doAdd)
    | opCode op == 2 = (pos+4, doBiFunc pos state (parameterModes op) doMul)
    | otherwise = error ("unsupported operation " ++ (show (opCode op)))

runOpWithoutIO :: Int -> Operation -> [Int] -> (Int, IO [Int])
runOpWithoutIO pos op state = let (newPos, newState) = runOp pos op state in (newPos, return newState)

runOpWithIO :: Int -> Operation -> [Int] -> (Int, IO [Int])
runOpWithIO pos op state
    | opCode op == 3 = (pos+2, inputInstr pos state)
    | opCode op == 4 = (pos+2, outputInstr pos state)
    | otherwise = runOpWithoutIO pos op state

runWithIO :: Int -> IO [Int] -> IO [Int]
runWithIO pos state = do
    s <- state
    let op = parseOperation (s!!pos)
        (nextPos, nextState) = runOpWithIO pos op s
    if (nextPos < 0) then nextState
    else runWithIO nextPos nextState

run :: Int -> [Int] -> [Int]
run pos state = let (nextPos, nextState) = runOp pos (parseOperation (state!!pos)) state in
    if (nextPos < 0) then nextState
    else run nextPos nextState
