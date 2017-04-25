module Main where

import DrawLogic
import FileInput
import DotInput

main :: IO ()
main = do
   src <- readFile "dotfile.txt"
   drawField (readDotFile src)
