module Operation 
  (
    Operation (..)
  , parseOperation
  ) where

import ParameterMode

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
