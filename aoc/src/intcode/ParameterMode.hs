module ParameterMode
  (
    ParameterMode (..)
  , toMode
  ) where

data ParameterMode 
    = Default
    | Position 
    | Immediate 
    | Relative
    deriving (Eq, Show)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative
toMode x = error $ "unsupported parameter mode " ++ (show x)
