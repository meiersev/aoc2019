module Main(main) where

import IntcodeSpec as IS
import UtilsSpec as US
import Test.Hspec

main :: IO ()
main = hspec $ do 
    US.spec
    IS.spec
