module DotInput where 

import Types
--import Data.Time.Clock
--import Data.Time.LocalTime


-- | Основная функция, переводящая псевдографический файл в игровое поле.
readDotFile :: String -> Board
readDotFile str = Board 
    { fieldHeight = x
    , fieldWidth = y
    , horizontal = reverse (getStrings (lines str))
    , vertical = reverse (getStrings(getColumns (lines str)))
    , field = replicate x (replicate y Nothing)
    , buttonPressed = False
    , difficulty = Nothing
    , colsToSee = []
    , rowsToSee = []
    , showMenu = False
--    , startTime = getTime      
    }
      where 
        (x, y) = getSize (lines str)
{-
getTime :: TimeOfDay
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now 
-}

getStrings :: [String] -> [[Int]]
getStrings strs = map parseFile1 strs

-- | Перевод одной строки в список чисел. 
parseFile1 :: String -> [Int]
parseFile1 str = map length (filter (\x -> head x == '#')(filter (\x -> x /= [])(group str))) 

-- | Группировка одинаковых элементов.
group :: Eq a => [a] -> [[a]]
group [] = [[]]
group (x:[]) = ((x:[]):[])
group (x:xs) = l:(group l2)
    where
      (l,l2) = gr ((x:[]), xs)

-- | Вспомогательная функция. Отделяет группы одинаковых элементов от списка.       
gr :: Eq a => ([a], [a]) -> ([a], [a])
gr (l1, []) = (l1, [])
gr (l1, (x:[])) |(head l1) == x = gr(x:l1, [])
              | otherwise = ((reverse l1), (x:[]))
gr (l1, (x:xs)) |(head l1) == x = gr(x:l1, xs)
                | otherwise = ((reverse l1), (x:xs))

{-
parseFile :: String -> [Int] -> [Int]
parseFile [] arr = (reverse arr)
parseFile str arr | (head str == 'x') = parseFile s1 (a1:arr)
                  | otherwise = parseFile (tail str) arr
    where
      (a1, s1) = cntX (0, str)


cntX :: (Int, String) -> (Int, String)
cntX (n, str) | (head str == 'x') = cntX (n+1, tail str)
              | otherwise = (n, tail str)
-}

getColumns :: [String] -> [String]
getColumns ([]:xs) = [[]]
getColumns l = (getFirst l):(getColumns (getTail l))

getSize :: [String] -> (Int, Int)
getSize (l:ls) = ((length (l:ls)), (length l))

-- | Список из первых элементов всех списков.                                      
getFirst :: [[a]] -> [a]
getFirst l | length l == 1 = (head (head l) : [])
           | otherwise = head (head l) : getFirst (tail l)

-- | Список из всех списков без их первых элементов.       
getTail :: [[a]] -> [[a]]
getTail l | length l == 1 = (tail (head l) : [])
          | otherwise = tail (head l) : getTail (tail l)


-- | Печать полученного представления файла в виде списка чисел.              
printInts :: [[Int]] -> String
printInts arr = unlines (map backToStr arr)               

-- | Перевод списка чисел в строку, пригодную для печати.     
backToStr :: [Int] -> String
backToStr arr = foldMap (\x -> show x ++ " ") arr     
              
