module Utils(replace) where

-- replace position n in the list with val
replace :: Int -> a -> [a] -> [a]
replace n val state = (take n state) ++ [val] ++ (drop (n + 1) state)
