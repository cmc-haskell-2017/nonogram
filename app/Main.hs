-- | Основной модуль.
module Main where

import DrawLogic
import FileInput (readFromFile)
--import DotInput (readDotFile)

main :: IO ()
main = do
    src <- readFile "house.txt"
    drawField (readFromFile src)
