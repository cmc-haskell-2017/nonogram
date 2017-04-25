module Main where

import DrawLogic
import FileInput
import DotInput
import SolverInterface

main :: IO ()
main = do
   src <- readFile "dotfile.txt" -- "umbr.txt"
   drawField (readDotFile src) --readFromFile src
    
