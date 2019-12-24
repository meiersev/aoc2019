module Intcode(run, runWithIO, runWithIOStub) where 

import Utils

data ParameterMode = Position | Immediate deriving (Eq, Show)
toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode x = error $ "unsupported parameter mode " ++ (show x)

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

valAt :: Int -> [Int] -> ParameterMode -> Int
valAt pos state Position = state!!(state!!pos)
valAt pos state Immediate = state!!pos

doOp :: Int -> [Int] -> [ParameterMode] -> (Int -> Int -> Int) -> Int
doOp pos state modes biFunc = let
    paddedModes = padParameterModes 2 modes
    in (valAt (pos+1) state $ head paddedModes) `biFunc` (valAt (pos+2) state $ last paddedModes)

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
    return $ replace (valAt (pos+1) state Immediate) i state

outputInstr :: Int -> [Int] -> ParameterMode -> IO [Int]
outputInstr pos state mode = do
    print $ valAt (pos+1) state mode
    return state

doJumpIfTrue :: Int -> [Int] -> [ParameterMode] -> (Int, [Int])
doJumpIfTrue pos state modes = doJumpIf pos state modes (\x -> x /= 0)

doJumpIfFalse :: Int -> [Int] -> [ParameterMode] -> (Int, [Int])
doJumpIfFalse pos state modes = doJumpIf pos state modes (\x -> x == 0)

doJumpIf :: Int -> [Int] -> [ParameterMode] -> (Int -> Bool) -> (Int, [Int])
doJumpIf pos state modes test = let
    paddedModes = padParameterModes 2 modes
    doJump = test $ valAt (pos+1) state $ head paddedModes
    in if doJump then (valAt (pos+2) state $ last paddedModes, state) else (pos + 3, state)

doLessThan pos state modes = doTest pos state modes (<) 
doEquals pos state modes = doTest pos state modes (==)

doTest :: Int -> [Int] -> [ParameterMode] -> (Int -> Int -> Bool) -> [Int]
doTest pos state modes test = let
    paddedModes = padParameterModes 2 modes
    set = test (valAt (pos+1) state $ head paddedModes) (valAt (pos+2) state $ last paddedModes)
    val = if set then 1 else 0
    in replace (state!!(pos+3)) val state 

runOp :: Int -> Operation -> [Int] -> (Int, [Int])
runOp pos op state
    | opCode op == 99 = (-1, state)
    | opCode op == 1 = (pos+4, doBiFunc pos state (parameterModes op) doAdd)
    | opCode op == 2 = (pos+4, doBiFunc pos state (parameterModes op) doMul)
    | opCode op == 5 = doJumpIfTrue pos state (parameterModes op)
    | opCode op == 6 = doJumpIfFalse pos state (parameterModes op)
    | opCode op == 7 = (pos+4, doLessThan pos state (parameterModes op))
    | opCode op == 8 = (pos+4, doEquals pos state (parameterModes op))
    | otherwise = error $ "unsupported operation " ++ (show $ opCode op)

runOpWithoutIO :: Int -> Operation -> [Int] -> (Int, IO [Int])
runOpWithoutIO pos op state = let (newPos, newState) = runOp pos op state in (newPos, return newState)

runOpWithIO :: Int -> Operation -> [Int] -> (Int, IO [Int])
runOpWithIO pos op state
    | opCode op == 3 = (pos+2, inputInstr pos state)
    | opCode op == 4 = (pos+2, outputInstr pos state $ head $ parameterModes op)
    | otherwise = runOpWithoutIO pos op state

runOpWithIOStub :: Int -> Operation -> [Int] -> [Int] -> (Int, [Int], [Int], [Int])
runOpWithIOStub pos op state inputs
    | opCode op == 3 = (pos+2, replace (valAt (pos+1) state Immediate) (head inputs) state, [], tail inputs)
    | opCode op == 4 = (pos+2, state, [valAt (pos+1) state $ head $ padParameterModes 1 $ parameterModes op], inputs)
    | otherwise = let (np, ns) = runOp pos op state in (np, ns, [], inputs)

-- do not do IO operations, instead use inputs from provided list, return outputs
runWithIOStub :: Int -> [Int] -> [Int] -> [Int]
runWithIOStub pos state inputs = runWithIOStubImpl pos state inputs []

runWithIOStubImpl :: Int -> [Int] -> [Int] -> [Int] -> [Int]
runWithIOStubImpl pos state inputs outputs = let 
    (nextPos, nextState, nextOuts, nextIns) = runOpWithIOStub pos (parseOperation $ state!!pos) state inputs
    accOutputs = outputs ++ nextOuts
    in 
        if (nextPos < 0) then accOutputs
        else runWithIOStubImpl nextPos nextState nextIns accOutputs

runWithIO :: Int -> IO [Int] -> IO [Int]
runWithIO pos state = do
    s <- state
    let op = parseOperation $ s!!pos
        (nextPos, nextState) = runOpWithIO pos op s
    if nextPos < 0 then nextState
    else runWithIO nextPos nextState

run :: Int -> [Int] -> [Int]
run pos state = let (nextPos, nextState) = runOp pos (parseOperation $ state!!pos) state in
    if (nextPos < 0) then nextState
    else run nextPos nextState
