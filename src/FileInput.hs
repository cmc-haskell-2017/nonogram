module FileInput(readFromFile) where

import Types
--import Data.Time.Clock
--import Data.Time.LocalTime

-- | Функция, создающая игровое поле по содержимому файла.       
readFromFile :: String -> Board
readFromFile st = Board
    { fieldHeight = x
    , fieldWidth = y
    , horizontal = tail(reverse l1)
    , vertical = (reverse l2)
    , field = replicate x (replicate y Nothing)
    , buttonPressed = False
    , difficulty = Nothing
--    , startTime = getTime    
    }
      where
        ((x, y), (l1, l2)) = divideString (parseString(lines st))

{- 
getTime :: TimeOfDay
getTime = do
   now <- getCurrentTime
   timezone <- getCurrentTimeZone
   return (localTimeOfDay $ utcToLocalTime timezone now)
--   return (TimeOfDay hour min sec)
-}            
-- | Получить числа в той же структуре, в которой они были записаны в файле.            
parseString :: [String] -> [[Int]]
parseString = (map (map (\x -> read x :: Int))).(map words)         

-- | Перевод списка чисел в удобное для разбора представление 
-- (отделяем первые 2 числа, обозначающие размеры поля)             
divideString :: [[Int]] -> ((Int,Int), ([[Int]], [[Int]]))
divideString lst = ((x, y), getLists (x,y) ss) 
    where
      ss = tail lst 
      x = head (head lst) 
      y = head (tail (head lst))
       
-- | По известным размерам поля делим список на числа, 
-- расположенные по горизонтали и по вертикали.
getLists :: (Int, Int) -> [[Int]] -> ([[Int]], [[Int]])
getLists (0, 0) _ = ([[]], [[]])
getLists (0, m) arr = ([[]], ((head arr):l2))
    where 
      (_, l2) = getLists (0, m-1) (tail arr)
getLists (n,m) arr = (((head arr):l1), l2)
    where
      (l1, l2) = getLists (n-1, m) (tail arr)


