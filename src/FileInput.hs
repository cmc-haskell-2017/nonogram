module FileInput(readFromFile) where

import Types
        
readFromFile :: String -> Board
readFromFile st = Board
    { fieldHeight = x
    , fieldWidth = y
    , horizontal = l1
    , vertical = l2
    , field = replicate x (replicate y Nothing)
    }
      where
        ((x, y), (l1, l2)) = parseString (pS1(lines st))
            
            
pS1 :: [String] -> [[Int]]
pS1 = (map (map (\x -> read x :: Int))).(map words)         
            
parseString :: [[Int]] -> ((Int,Int), ([[Int]], [[Int]]))
parseString lst = ((x, y), getLists (x,y) ss) 
    where
      ss = tail lst 
      x = head (head lst) 
      y = head (tail (head lst))
       

getLists :: (Int, Int) -> [[Int]] -> ([[Int]], [[Int]])
getLists (0, 0) _ = ([[]], [[]])
getLists (0, m) arr = ([[]], ((head arr):l2))
    where 
      (_, l2) = getLists (0, m-1) (tail arr)
getLists (n,m) arr = (((head arr):l1), l2)
    where
      (l1, l2) = getLists (n-1, m) (tail arr)


