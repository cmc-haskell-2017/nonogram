-- | Основная функция, переводящая псевдографический файл в список списков чисел.
readDotFile :: String -> [[Int]]
readDotFile str = map parseFile1 (lines str)

-- | Перевод одной строки в список чисел. 
parseFile1 :: String -> [Int]
parseFile1 str = map length (filter (\x -> head x == 'x')(filter (\x -> x /= [])(group str))) 

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

-- | Печать полученного представления файла в виде списка чисел.              
printInts :: [[Int]] -> String
printInts arr = unlines (map backToStr arr)               

-- | Перевод списка чисел в строку, пригодную для печати.     
backToStr :: [Int] -> String
backToStr arr = foldMap (\x -> show x ++ " ") arr     
              
main :: IO()
main = do 
    src <- readFile "dotfile.txt"
    putStrLn(printInts (readDotFile src))
