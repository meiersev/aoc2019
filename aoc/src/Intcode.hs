module Intcode
  (
    Program
  , ProgramStatus (..)
  , ProgramContext (..)

  , fromIntList
  , readState
  , run
  , runSimple
  , runWithInputs
  ) where 

import qualified Data.IntMap.Strict as IntMap

type Program = [Int]
type State = IntMap.IntMap Int

fromIntList :: Program -> State
fromIntList xs = IntMap.fromDistinctAscList $ zip [0..(length xs -1)] xs

data ProgramStatus 
    = Ready 
    | Suspended 
    | Finished
    deriving (Eq, Show)

data ProgramContext = ProgramContext 
    { prog :: State
    , pos :: Int
    , inputs :: [Int]
    , outputs :: [Int]
    , status :: ProgramStatus
    , relBase :: Int
    } deriving Show

data ParameterMode 
    = Position 
    | Immediate 
    | Relative
    deriving (Eq, Show)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative
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
nextOperation ctx = parseOperation $ valAt ctx (pos ctx) Immediate

padParameterModes :: Int -> [ParameterMode] -> [ParameterMode]
padParameterModes n modes = let
    paddingSize = n - (length modes)
    in modes ++ (replicate paddingSize Position)

store :: Int -> Int -> State -> State
store adr val prog = IntMap.insert adr val prog

readState :: Int -> ProgramContext -> Int
readState at ctx = valAt ctx at Immediate

valAt :: ProgramContext -> Int -> ParameterMode -> Int
valAt ctx pos mode
    | mode == Position = withDefault instrVal
    | mode == Immediate = instrVal
    | mode == Relative = withDefault $ instrVal + (relBase ctx)
  where
    withDefault at = IntMap.findWithDefault 0 at $ prog ctx
    instrVal = withDefault pos

doAdd ctx = doBinOp ctx (+)
doMul ctx = doBinOp ctx (*)

doBinOp :: ProgramContext -> (Int -> Int -> Int) -> ProgramContext
doBinOp ctx binOp = ctx { prog = store resultAddr result $ prog ctx, pos = pos'+4 }
  where
    pos' = pos ctx
    paddedModes = padParameterModes 2 $ parameterModes $ nextOperation ctx
    resultAddr = valAt ctx (pos'+3) Immediate
    operand1 = valAt ctx (pos'+1) $ head paddedModes
    operand2 = valAt ctx (pos'+2) $ last paddedModes
    result = operand1 `binOp` operand2

doJumpIfTrue ctx = doJumpIf ctx (\x -> x /= 0)
doJumpIfFalse ctx = doJumpIf ctx (\x -> x == 0)

doJumpIf :: ProgramContext -> (Int -> Bool) -> ProgramContext
doJumpIf ctx test =
    if doJump
    then ctx { pos = valAt ctx (pos'+2) $ last paddedModes }
    else ctx { pos = pos'+3 }
  where
    pos' = pos ctx
    paddedModes = padParameterModes 2 $ parameterModes $ nextOperation ctx
    doJump = test $ valAt ctx (pos'+1) $ head paddedModes

doLessThan ctx = doTest ctx (<) 
doEquals ctx = doTest ctx (==)

doTest :: ProgramContext -> (Int -> Int -> Bool) -> ProgramContext
doTest ctx test = ctx { pos = (pos ctx) + 4, prog = newState }
  where
    paddedModes = padParameterModes 2 $ parameterModes $ nextOperation ctx
    pos' = pos ctx
    set = test (valAt ctx (pos'+1) $ head paddedModes) (valAt ctx (pos'+2) $ last paddedModes)
    val = if set then 1 else 0
    state = prog ctx
    newState = store (valAt ctx (pos'+3) Immediate) val state 

doRead :: ProgramContext -> ProgramContext
doRead ctx =
    if (length $ inputs ctx) > 0
    then ctx { prog = nextState, pos = pos'+2, inputs = tail inputs' }
    else ctx { status = Suspended }
  where 
    pos' = pos ctx
    inputs' = inputs ctx
    nextState = store (valAt ctx (pos'+1) Immediate) (head inputs') $ prog ctx

doWrite :: ProgramContext -> ProgramContext
doWrite ctx = ctx { pos = pos'+2, outputs = output:(outputs ctx) }
  where
    pos' = pos ctx
    output = valAt ctx (pos'+1) $ head $ padParameterModes 1 $ parameterModes $ nextOperation ctx

doChangeRelBase :: ProgramContext -> ProgramContext
doChangeRelBase ctx = ctx { pos = pos'+2, relBase = (relBase ctx) + valAt ctx (pos'+1) Immediate }
  where
    pos' = pos ctx

runOp :: ProgramContext -> ProgramContext
runOp ctx = case opCode $ nextOperation ctx of
    99 -> ctx { status = Finished }
    1 -> doAdd ctx 
    2 -> doMul ctx
    3 -> doRead ctx
    4 -> doWrite ctx
    5 -> doJumpIfTrue ctx
    6 -> doJumpIfFalse ctx
    7 -> doLessThan ctx
    8 -> doEquals ctx
    9 -> doChangeRelBase ctx
    _ -> error $ "unsupported operation " ++ (show $ opCode $ nextOperation ctx)

runSimple :: [Int] -> ProgramContext
runSimple prog = runWithInputs prog []

runWithInputs :: [Int] -> [Int] -> ProgramContext
runWithInputs prog ins = run $ ProgramContext progMap 0 ins [] Ready 0
  where
    progMap = fromIntList prog

run :: ProgramContext -> ProgramContext
run ctx = case status ctx of 
    Ready -> run $ runOp ctx
    _ -> ctx
