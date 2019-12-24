module Day7 where

import Data.List
import Utils
import Intcode

phases :: [[Int]]
phases = permutations [0..4]

testInput :: Program
testInput = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 
testInput2 :: Program
testInput2 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

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

input :: IO [Int]
input = do
    i <- readFile inputFile
    let split = splitString i ','
    let prog = map (\s -> read s :: Int) split
    return prog

runPart1 :: IO ()
runPart1 = do
    prog <- input
    print $ tryAllPhases phases prog

-- Part 2
phasesPt2 :: [[Int]]
phasesPt2 = permutations [5..9]

initAmplifiers :: Program -> [Int] -> [ProgramContext]
initAmplifiers prog phases = ampA : amps
  where
    ampA = ProgramContext prog 0 [head phases, 0] [] Ready 0
    amps = map (\p -> ProgramContext prog 0 [p] [] Ready 0) $ tail phases

anyAmpWithStatus s = any (\amp -> status amp == s)

moveOutputToInputs :: [ProgramContext] -> [ProgramContext]
moveOutputToInputs ctxs = map clearOutputs $ map updateInputs zipped
  where
    zipped = zip (rotateList 1 ctxs) ctxs
    updateInputs (from,to) = 
        if (length $ outputs from) > 0 
        then to { inputs = [last $ outputs from], status=Ready } 
        else to
    clearOutputs ctx = ctx { outputs=[] }

runAmpsToCompletion :: [ProgramContext] -> [ProgramContext]
runAmpsToCompletion amps = 
    if anyAmpWithStatus Ready amps
    then runAmpsToCompletion $ map run amps
    else
        if anyAmpWithStatus Suspended amps
        then runAmpsToCompletion $ moveOutputToInputs amps
        else amps -- all finished

thrusterSignal :: [ProgramContext] -> Int
thrusterSignal ctxs = head $ outputs $ last ctxs

maxSignalFromPhase :: [[Int]] -> Program -> (Int, [Int])
maxSignalFromPhase (p:[]) prog = (thrusterSignal $ runAmpsToCompletion $ initAmplifiers prog p, p) 
maxSignalFromPhase (p:ps) prog = 
    if thisSignal > fst other
    then (thisSignal , p)
    else other
  where
    ctxs = initAmplifiers prog p
    thisSignal = thrusterSignal $ runAmpsToCompletion ctxs 
    other = maxSignalFromPhase ps prog

runPart2 :: IO ()
runPart2 = do
    prog <- input
    print $ maxSignalFromPhase phasesPt2 prog
    return ()
