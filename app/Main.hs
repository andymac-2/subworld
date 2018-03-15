module Main where

import qualified Compiler as C
import qualified Data.Text.IO as T
import System.Environment (getArgs);

main :: IO ()
main = T.interact C.compile 
