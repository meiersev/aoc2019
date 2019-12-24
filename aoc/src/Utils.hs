module Utils
  (
    replace
  , splitString
  , rotateList
  ) where

-- replace position n in the list with val
replace :: Int -> a -> [a] -> [a]
replace n val state = (take n state) ++ [val] ++ (drop (n + 1) state)

-- split string at delimiter
splitString :: String -> Char -> [String]
splitString "" _ = []
splitString str delimiter = let 
    testDel = (\c -> c /= delimiter)
    firstMatch = takeWhile testDel str
    restOfString = (dropWhile testDel str)
    rest = if (length restOfString > 0) then splitString (tail restOfString) delimiter else []
    in firstMatch : rest

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = take l $ drop fst $ cycle xs
  where 
    l = length xs
    fst = l - (mod n l)
