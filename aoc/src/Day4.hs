module Day4(printResult) where
-- inputLow = 0
inputLow = 134564
inputHigh = 585159

hasEqualAdjacent :: (Eq a) => Int -> [a] -> Bool
hasEqualAdjacent n ls = any (\el -> length el == n) (partitionList ls)

startsWithN :: (Eq a) => Int -> a -> [a] -> Bool
startsWithN 0 _ _ = True
startsWithN _ _ [] = False
startsWithN 1 fstEl (x:y:xs) = (x == fstEl) && (y /= fstEl)
startsWithN n fstEl (x:xs) = x == fstEl && (startsWithN (n-1) fstEl xs)

partitionList :: (Eq a) => [a] -> [[a]]
partitionList [] = [] 
partitionList l@(x:xs) = (takeWhile ((==) x) l) : (partitionList (dropWhile ((==) x) l))

adjacentGrouped :: [a] -> [(a, a)]
adjacentGrouped [] = []
adjacentGrouped (x:[]) = []
adjacentGrouped (x:y:ys) = (x, y) : (adjacentGrouped (y:ys))

is6Digits :: Int -> Bool
is6Digits x = (length (show x)) == 6

inRange :: Int -> Bool
inRange x = inputLow <= x && x <= inputHigh

hasEqualAdjacentDigits :: Int -> Bool
hasEqualAdjacentDigits x = any (\(a,b) -> a == b) (adjacentGrouped (asDigits x))

digitsAscending :: Int -> Bool
digitsAscending x = foldl (&&) True (map (\(a,b) -> a <= b) (adjacentGrouped (asDigits x)))

asDigits :: Int -> [Int]
asDigits x = map (\intStr -> read [intStr] :: Int) (show x)

validPwd :: Int -> Bool
validPwd x = and [is6Digits x, inRange x, hasEqualAdjacent 2 (show x), digitsAscending x]

nValidInRange = length (filter validPwd [inputLow..inputHigh])

printResult = print nValidInRange

