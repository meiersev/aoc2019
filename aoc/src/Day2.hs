module Day2(result) where
-- changed position 1 from 0 to 12, position 2 from 0 to 2:
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,13,27,1,6,27,31,1,9,31,35,2,10,35,39,1,39,6,43,1,6,43,47,2,13,47,51,1,51,6,55,2,6,55,59,2,59,6,63,2,63,13,67,1,5,67,71,2,9,71,75,1,5,75,79,1,5,79,83,1,83,6,87,1,87,6,91,1,91,5,95,2,10,95,99,1,5,99,103,1,10,103,107,1,107,9,111,2,111,10,115,1,115,9,119,1,13,119,123,1,123,9,127,1,5,127,131,2,13,131,135,1,9,135,139,1,2,139,143,1,13,143,0,99,2,0,14,0]
-- input = [1,0,0,0,99]
-- input = [2,3,0,3,99]
-- input = [1,1,1,4,99,5,6,0,99]

replace :: Int -> Int -> [Int] -> [Int]
replace n val state = (take n state) ++ [val] ++ (drop (n + 1) state)

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


target = 19690720
possibilities = [0..99]
nounVerbCombos = [(a, b) | a <- possibilities, b <- possibilities]

startConfig :: (Int, Int) -> [Int]
startConfig (a,b) = replace 1 a (replace 2 b input)

progResults :: [(Int, Int, Int)]
progResults = [(a, b, head (run 0 (startConfig sc))) | sc@(a,b) <- nounVerbCombos]

winnerCombo :: [(Int, Int, Int)] -> (Int, Int)
winnerCombo [] = (-1, -1)
winnerCombo ((a, b, c):xs)
    | c == target = (a, b)
    | otherwise = winnerCombo xs

result :: Int
result = let (noun, verb) = winnerCombo progResults in 100 * (noun) + (verb)

