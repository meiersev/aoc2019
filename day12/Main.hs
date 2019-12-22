-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>
moonNames :: [String]
moonNames = ["io", "eu", "ga", "ca"]
input :: [(Int, Int, Int)]
-- input = [(-1,0,2), (2,-10,-7), (4,-8,8), (3,5,-1)] -- test input
input = [(-5,6,-11), (-8,-4,-2), (1,16,4), (11,11,-4)]

data Vector = Vector {x :: Int, y :: Int, z :: Int}
data Moon = Moon {name :: String, pos :: Vector, vel :: Vector}

instance Show Vector where
    show (Vector x y z) = "<x=" ++ show x ++ ",y=" ++ show y ++ ",z=" ++ show z ++ ">"

instance Show Moon where
    show (Moon name p v) = show name ++ ": pos=" ++ show p ++ ", vel=" ++ show v

instance Eq Moon where
    (==) (Moon n1 _ _) (Moon n2 _ _) = n1 == n2

vadd :: Vector -> Vector -> Vector
vadd (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z)

moons :: [Moon]
moons = map (\(n, t) -> initMoon n t) (zip moonNames input)

initMoon :: String -> (Int, Int, Int) -> Moon
initMoon name (x,y,z) = Moon name (Vector x y z) (Vector 0 0 0)

velocityChange :: Int -> Int -> Int -> Int
velocityChange vel a b
    | a < b = vel + 1
    | a > b = vel - 1
    | otherwise = vel

updateFstVelocity :: (Moon, [Moon]) -> Moon
updateFstVelocity (m, []) = m
updateFstVelocity (m, (n:ns)) = let
    newXVel = velocityChange ((x.vel) m) ((x.pos) m) ((x.pos) n)
    newYVel = velocityChange ((y.vel) m) ((y.pos) m) ((y.pos) n)
    newZVel = velocityChange ((z.vel) m) ((z.pos) m) ((z.pos) n)
    updatedMoon = Moon (name m) (pos m) (Vector newXVel newYVel newZVel)
    in
        updateFstVelocity (updatedMoon, ns)

updatedVelocities :: [Moon] -> [Moon]
updatedVelocities mns = map updateFstVelocity (opposeOthers mns)

opposeOthers :: (Eq a) => [a] -> [(a, [a])]
opposeOthers ls = [(x, [y | y <- ls, y /= x]) | x <- ls]

applyVelocity :: Moon -> Moon
applyVelocity m = Moon (name m) (vadd (pos m) (vel m)) (vel m)

updatedPositions :: [Moon] -> [Moon]
updatedPositions mns = map applyVelocity mns

updated :: [Moon] -> [Moon]
updated = updatedPositions.updatedVelocities

iterateUpdate :: Int -> [Moon] -> [Moon]
iterateUpdate 0 mns = mns 
iterateUpdate i mns = iterateUpdate (i-1) (updated mns)

sumAbsVector vec = foldl (+) 0 (map (\f -> (abs.f) vec) [x, y, z])
potentialEnergy :: Moon -> Int
potentialEnergy m = sumAbsVector (pos m)

kineticEnergy :: Moon -> Int
kineticEnergy m = sumAbsVector (vel m)

energy :: Moon -> Int
energy m = (potentialEnergy m) * (kineticEnergy m)

totalEnergy :: [Moon] -> Int
totalEnergy mns = foldl (+) 0 (map energy mns)

printMoons [] = do
    putStrLn ""
printMoons (m:mns) = do
    putStrLn (show m)
    printMoons mns

main1 = do
    let endState = iterateUpdate 1000 moons
    let energy = totalEnergy endState
    printMoons endState
    putStrLn (show energy)

-- Part 2
isSameState :: [Moon] -> (Vector -> Int) -> Bool
isSameState mns acc = map acc (map pos moons) == map acc (map pos mns) && map acc (map vel moons) == map acc (map vel mns)

stepsUntilEq = iterateUntilEq (-1, -1, -1) (updated moons) 1

iterateUntilEq :: (Int, Int, Int) -> [Moon] -> Int -> (Int, Int, Int)
iterateUntilEq res@(a,b,c) mns stepCtr
    | a > -1 && b > -1 && c > -1 = res
    | a == -1 && isSameState mns x = iterateUntilEq (stepCtr, b, c) mns stepCtr
    | b == -1 && isSameState mns y = iterateUntilEq (a, stepCtr, c) mns stepCtr
    | c == -1 && isSameState mns z = iterateUntilEq (a, b, stepCtr) mns stepCtr
    | otherwise = iterateUntilEq res (updated mns) (stepCtr + 1)

main = do
    let sue@(a,b,c) = stepsUntilEq
    print sue
    print (foldl lcm 1 [a,b,c])
