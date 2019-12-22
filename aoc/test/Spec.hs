module Main(main) where

import IntcodeSpec as IS
import Test.Hspec

main :: IO ()
main = hspec $ IS.spec
