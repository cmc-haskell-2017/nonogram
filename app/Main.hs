module Main where

import DrawLogic
import FileInput

main :: IO ()
main = do
   src <- readFile "umbr.txt"
   drawField (readFromFile src)
    
