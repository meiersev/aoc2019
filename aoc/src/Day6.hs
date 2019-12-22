module Day6(printResult) where

import Data.Map.Strict hiding (map)

inputFile = "test/resources/day6/input"

data Object = COM | Object { name :: String, center :: String } deriving Show

separator = (/=) ')'

parseSingle :: String -> Object
parseSingle str = let 
    center = takeWhile separator str
    name = tail (dropWhile separator str)
    in Object name center

parseObjects :: [String] -> Map String Object
parseObjects ls = let fromIn = fromList (map (\str -> let obj = parseSingle str in (name obj, obj)) ls) in
    insert "COM" COM fromIn

nOrbitsClosure :: Object -> Map String Object -> Int
nOrbitsClosure COM _ = 0
nOrbitsClosure obj m = 1 + nOrbitsClosure (m ! (center obj)) m

pathToRoot :: Map String Object -> String -> [String]
pathToRoot _ "COM" = []
pathToRoot m key = let cent = center (m ! key) in cent : pathToRoot m cent

lca :: Map String Object -> String -> String -> String
lca m a b = let 
    pTa = pathToRoot m a
    pTb = pathToRoot m b
    zippedPaths = zip (reverse pTa) (reverse pTb)
    firstDiff = head (dropWhile (\(x, y) -> x == y) zippedPaths)
    in
        center (m ! (fst firstDiff))

jumps :: Map String Object -> String -> String -> Int
jumps m a b = let
    pTa = pathToRoot m a
    pTb = pathToRoot m b
    lcaAB = lca m a b
    distToLca = \path -> length (takeWhile ((/=) lcaAB) path)
    in
        distToLca pTa + distToLca pTb

printResult = do
    input <- readFile inputFile
    let m = parseObjects (lines input)
    -- print (toList m)
    let resultPart1 = sum (map (\el -> nOrbitsClosure el m) (elems m))
    print resultPart1
    print (jumps m "YOU" "SAN")

