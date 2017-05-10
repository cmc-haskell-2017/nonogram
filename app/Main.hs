module Main where

import DrawLogic
import FileInput
import DotInput
import SolverInterface

main :: IO ()
main = do
   src <- readFile "horse_d2.txt"
{-   putStrLn (dif src)
     where
       dif src | getDifficulty (readFromFile src) == Nothing = "Unsolvable"
               | otherwise = takeDif (getDifficulty (readFromFile src))
         where
           takeDif (Just n) = "Difficulty: " ++ (show n)
-}
   drawField (readFromFile src)
