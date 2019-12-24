module Intcode
  (
    Program
  , ProgramStatus
  , ProgramContext

  , prog
  , outputs

  , run
  , runSimple
  , runWithInputs
  ) where 

import Utils

type Program = [Int]

data ProgramStatus 
    = Ready 
    | Suspended 
    | Finished
    deriving Eq

data ProgramContext = ProgramContext 
    { prog :: Program
    , pos :: Int
    , inputs :: [Int]
    , outputs :: [Int]
    , status :: ProgramStatus
    }

data ParameterMode 
    = Position 
    | Immediate 
    deriving (Eq, Show)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode x = error $ "unsupported parameter mode " ++ (show x)

data Operation = Operation 
    { opCode :: Int
    , parameterModes :: [ParameterMode]
    } deriving Show

parseOperation :: Int -> Operation
parseOperation x = let 
    digits = show x
    (modeDigits, opCodeDigits) = splitAt (length digits - 2) digits
    opCode = read opCodeDigits
    modes = map (toMode.read.(\c -> [c])) (reverse modeDigits) 
    in Operation opCode modes

nextOperation :: ProgramContext -> Operation
nextOperation (ProgramContext prog pos _ _ _) = parseOperation $ prog!!pos

padParameterModes :: Int -> [ParameterMode] -> [ParameterMode]
padParameterModes n modes = let
    paddingSize = n - (length modes)
    in modes ++ (replicate paddingSize Position)

valAt :: Int -> Program -> ParameterMode -> Int
valAt pos state Position = state!!(state!!pos)
valAt pos state Immediate = state!!pos

doOp :: Int -> Program -> [ParameterMode] -> (Int -> Int -> Int) -> Int
doOp pos state modes biFunc = let
    paddedModes = padParameterModes 2 modes
    in (valAt (pos+1) state $ head paddedModes) `biFunc` (valAt (pos+2) state $ last paddedModes)

doAdd :: Int -> Program -> [ParameterMode] -> Int
doAdd pos state modes = doOp pos state modes (+)

doMul :: Int -> Program -> [ParameterMode] -> Int
doMul pos state modes = doOp pos state modes (*)

doBiFunc :: Int -> Program -> [ParameterMode] -> (Int -> Program -> [ParameterMode] -> Int) -> Program
doBiFunc pos state modes op = replace (state!!(pos+3)) (op pos state modes) state

inputInstr :: Int -> Program -> IO Program
inputInstr pos state = do
    input <- getLine
    let i = read input :: Int
    return $ replace (valAt (pos+1) state Immediate) i state

outputInstr :: Int -> Program -> ParameterMode -> IO Program
outputInstr pos state mode = do
    print $ valAt (pos+1) state mode
    return state

doJumpIfTrue pos state modes = doJumpIf pos state modes (\x -> x /= 0)
doJumpIfFalse pos state modes = doJumpIf pos state modes (\x -> x == 0)

doJumpIf :: Int -> Program -> [ParameterMode] -> (Int -> Bool) -> Int
doJumpIf pos state modes test =
    if doJump
    then valAt (pos+2) state $ last paddedModes
    else pos+3
  where
    paddedModes = padParameterModes 2 modes
    doJump = test $ valAt (pos+1) state $ head paddedModes

doLessThan pos state modes = doTest pos state modes (<) 
doEquals pos state modes = doTest pos state modes (==)

doTest :: Int -> Program -> [ParameterMode] -> (Int -> Int -> Bool) -> Program
doTest pos state modes test = let
    paddedModes = padParameterModes 2 modes
    set = test (valAt (pos+1) state $ head paddedModes) (valAt (pos+2) state $ last paddedModes)
    val = if set then 1 else 0
    in replace (state!!(pos+3)) val state 

runOp :: ProgramContext -> ProgramContext
runOp ctx@(ProgramContext state pos inputs outputs status) = case opCode $ nextOperation ctx of
    99 -> ProgramContext state (-1) inputs outputs Finished
    1 -> ProgramContext newState (pos+4) inputs outputs status 
      where 
        newState = doBiFunc pos state (parameterModes $ nextOperation ctx) doAdd
    2 -> ProgramContext newState (pos+4) inputs outputs status
      where
        newState = doBiFunc pos state (parameterModes $ nextOperation ctx) doMul
    3 -> 
        if length inputs > 0 
        then ProgramContext nextState (pos+2) (tail inputs) outputs status
        else ProgramContext state pos inputs outputs Suspended
      where 
        nextState = replace (valAt (pos+1) state Immediate) (head inputs) state 
    4 -> ProgramContext state (pos+2) inputs (output:outputs) status
      where 
        output = valAt (pos+1) state $ head $ padParameterModes 1 $ parameterModes $ nextOperation ctx
    5 -> ProgramContext state newPos inputs outputs status
      where
        newPos = doJumpIfTrue pos state (parameterModes $ nextOperation ctx)
    6 -> ProgramContext state newPos inputs outputs status
      where 
        newPos = doJumpIfFalse pos state (parameterModes $ nextOperation ctx)
    7 -> ProgramContext newState (pos+4) inputs outputs status
      where
        newState = doLessThan pos state (parameterModes $ nextOperation ctx)
    8 -> ProgramContext newState (pos+4) inputs outputs status
      where
        newState = doEquals pos state (parameterModes $ nextOperation ctx)
    _ -> error $ "unsupported operation " ++ (show $ opCode $ nextOperation ctx)

runSimple :: Program -> ProgramContext
runSimple prog = run $ ProgramContext prog 0 [] [] Ready

runWithInputs :: Program -> [Int] -> ProgramContext
runWithInputs prog ins = run $ ProgramContext prog 0 ins [] Ready

run :: ProgramContext -> ProgramContext
run ctx@(ProgramContext state pos _ _ _) = 
    if (status nextCtx) /= Ready 
    then nextCtx
    else run nextCtx
  where
    nextCtx = runOp ctx