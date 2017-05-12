module AutoSolver where

import Types
import Debug.Trace

type Line = ([Int], [Cell])

data SolveBoard = SolveBoard
    { playBoard :: Board
    , solvingSteps :: Int
    , linesSeen :: Int
    }


-- | Функция решателя, работающая, пока все клетки не будут заполнены и пока её раота изменяет хоть что-то.
autoSolve :: SolveBoard -> Board
autoSolve brd = countDifficulty (checkRows [0..(fieldWidth (playBoard brd))] brd)

-- | Функция решателя, работающая, пока все клетки не будут заполнены и пока её раота изменяет хоть что-то.
autoSolve1 :: SolveBoard -> Board
autoSolve1 brd | (rowsNext fld == True) && (r /= []) = (playBoard (checkRows1 r brd))
               | c /= [] = (playBoard (checkCols1 c brd))
               | otherwise = countDifficulty (checkRows1 [0..(fieldHeight fld)-1] brd{playBoard = fld{rowsToSee = [0..(fieldHeight fld)-1]
                                                                                                      , rowsNext = True}})
  where
    r = (rowsToSee (playBoard brd))
    c = (colsToSee (playBoard brd))
    fld = playBoard brd
    

-- | Возвращает головоломку с определённой сложностью.
countDifficulty :: SolveBoard -> Board
countDifficulty brd | (difficulty (playBoard brd)) /= Nothing = (playBoard brd) 
                    | (isSolved (field (playBoard brd))) == False = (playBoard brd){difficulty = Nothing}
                    | otherwise = (playBoard brd){difficulty = Just (rateGame (solvingSteps brd) (linesSeen brd) (playBoard brd))}

-- | Оценка сложности игры (количество звёздочек из трёх).                    
rateGame :: Int -> Int -> Board -> Int
rateGame n s brd | prcnt < 20 = 1
                 | prcnt > 32 = 3
                 | otherwise = 2
    where
      size = (fieldWidth brd) * (fieldHeight brd) -- отношение количества изменений столбцов/строк к общему количеству клеток также определяет сложность
      lng = max (fieldWidth brd) (fieldHeight brd) -- чем больше головоломка, тем она труднее
      prcnt = (60 - n * 60 `div` s) + n * 20 `div` size + lng `div` 10 -- первое слагаемое - отношение количества изменений в линиях к количеству просмотренных линий


-- | Проверка, всё ли поле заполнено.
isSolved :: [[Cell]] -> Bool
isSolved [] = True
isSolved [[]] = True
isSolved f | (rowFin (head f)) = isSolved (tail f)
           | otherwise = False
                
-- | Проверка, все ли клетки строки заполнены.
rowFin :: [Cell] -> Bool
rowFin [] = True
rowFin (x:xs) | (x == Nothing) = False
              | otherwise = rowFin xs


-- | Обработка всех строк из списка.
checkRows :: [Int] -> SolveBoard -> SolveBoard
checkRows [] brd = brd{playBoard = (playBoard brd){rowsNext = False}}
checkRows (x:xs) brd = checkRows xs (checkRow x True brd)


-- | Обработка всех строк из списка.
checkRows1 :: [Int] -> SolveBoard -> SolveBoard
checkRows1 [] brd = brd
checkRows1 (x:xs) brd | (solvingSteps n_brd) == 0 = checkRows1 xs n_brd
                      | otherwise = n_brd
  where 
    fld = playBoard brd
    n_brd = (checkRow x False brd{playBoard = fld{rowsToSee = xs}})
        
-- | Обработка строки с заданным номером.
checkRow :: Int -> Bool -> SolveBoard -> SolveBoard
checkRow n b brd | (rowFin ls) = brd
                 | b == True = placeRow brd (makeMask ns ls) n
                 | otherwise = placeRow1 brd (makeMask ns ls) n
    where
      (ns, ls) = getRow (playBoard brd) n

-- | Обработка всех столбцов из списка.
checkCols :: [Int] -> SolveBoard -> SolveBoard
checkCols [] brd = brd{playBoard = (playBoard brd){rowsNext = True}}
checkCols (x:xs) brd = checkCols xs (checkCol x True brd)

-- | Обработка всех столбцов из списка.
checkCols1 :: [Int] -> SolveBoard -> SolveBoard
checkCols1 [] brd = brd
checkCols1 (x:xs) brd | (solvingSteps n_brd) == 0 = checkCols1 xs n_brd
                      | otherwise = n_brd
  where 
    fld = playBoard brd
    n_brd = (checkCol x False brd{playBoard = fld{colsToSee = xs}})

-- | Получение списка измененных клеток в линии.
cellsChanged :: Int -> [Cell] -> [Cell] -> [Int] -> [Int]
cellsChanged _ [] [] l = l
cellsChanged n (x:xs) (y:ys) l | (eqCell x y) = cellsChanged (n+1) xs ys l
                               | otherwise = cellsChanged (n+1) xs ys (n:l)

-- | Обработка стобца с заданным номером.
checkCol :: Int -> Bool -> SolveBoard -> SolveBoard
checkCol n b brd | (rowFin ls) = brd
                 | b == True = placeCol brd (makeMask ns ls) n
                 | otherwise = placeCol1 brd (makeMask ns ls) n
    where
      (ns, ls) = getCol fld{vertical = reverse (vertical fld)} n
        where
          fld = playBoard brd

-- | Изменение строки с заданным номером, если необходимо.
placeRow :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeRow brd Nothing _ = brd
placeRow brd (Just m) n | (eqLine cs m) = brd{linesSeen = (linesSeen brd) + 1}
                        | otherwise = checkCols (cellsChanged 0 cs m []) (brd { playBoard = fld{field = putRow (field fld) m n}
                                                                       , solvingSteps = (solvingSteps brd) + 1
                                                                       , linesSeen = (linesSeen brd) + 1})
    where
      (_, cs) = getRow fld n
      fld = playBoard brd
      

-- | Изменение строки с заданным номером, если необходимо.
placeRow1 :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeRow1 brd Nothing _ = brd
placeRow1 brd (Just m) n | (eqLine cs m) = brd{linesSeen = (linesSeen brd) + 1}
                         | otherwise = brd { playBoard = fld{ field = putRow (field fld) m n
                                                           , colsToSee =  ((cellsChanged 0 cs m []) ++ l)
                                                           , rowsNext = False}
                                          , solvingSteps = (solvingSteps brd) + 1
                                          , linesSeen = (linesSeen brd) + 1}
    where
      (_, cs) = getRow fld n
      fld = playBoard brd
      l = (colsToSee fld)

-- | Добавление измененной строки к полю.
putRow :: [[Cell]] -> [Cell] -> Int -> [[Cell]]
putRow f l 0 = l : (tail f)
putRow f m n = (head f) : (putRow (tail f) m (n-1))                   


-- | Изменение столбца с заданным номером, если необходимо.
placeCol :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeCol brd Nothing _ = brd
placeCol brd (Just m) n | (eqLine cs m) = brd{linesSeen = (linesSeen brd) + 1}
                        | otherwise = checkRows (cellsChanged 0 cs m []) (brd { playBoard = fld{field = putCol (field fld) m n}
                                                                     , solvingSteps = (solvingSteps brd) + 1
                                                                     , linesSeen = (linesSeen brd) + 1})
    where
      (_, cs) = getCol fld n
      fld = playBoard brd
                
-- | Изменение столбца с заданным номером, если необходимо.
placeCol1 :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeCol1 brd Nothing _ = brd
placeCol1 brd (Just m) n | (eqLine cs m) = brd{linesSeen = (linesSeen brd) + 1}
                         | otherwise = brd { playBoard = fld{ field = putCol (field fld) m n
                                                            , rowsToSee = l ++ (cellsChanged 0 cs m [])
                                                            , rowsNext = True}
                                          , solvingSteps = (solvingSteps brd) + 1
                                          , linesSeen = (linesSeen brd) + 1}
    where
      (_, cs) = getCol fld n
      fld = playBoard brd
      l = (rowsToSee fld)
                 
-- | Добавление измененного столбца к полю.
putCol :: [[Cell]] -> [Cell] -> Int -> [[Cell]]
putCol [[]] _ _ = [[]]
putCol f l 0 = putFirst l (getTail f)
putCol f m n = putFirst (getFirst f) (putCol (getTail f) m (n-1))

-- | Проверка на равенство маски и ряда.
eqLine :: [Cell] -> [Cell] -> Bool
eqLine [] [] = True
eqLine (x:[]) (y:_) = (eqCell x y)
eqLine (x:xs) (y:ys) | (eqCell x y) = eqLine xs ys
                     | otherwise = False
                            
-- | Проверка клеток на равенство.
eqCell :: Cell -> Cell -> Bool  
eqCell (Just Y) (Just Y) = True
eqCell (Just N) (Just N) = True
eqCell Nothing Nothing = True
eqCell _ _ = False
      
-- | Создание маски ряда.
makeMask :: [Int] -> [Cell] -> Maybe [Cell]
makeMask [] lst | (checkEmpty lst) = Just (replicate (length lst) (Just N))
                | otherwise = Nothing
makeMask _ [] = Nothing
makeMask l1 (x:xs) | (canPlace (head l1) (x:xs)) && (x == (Just Y)) = maskAdd h t 
                   | (canPlace (head l1) (x:xs)) = maskAppend (maskAdd h t) (mList (Just N) (makeMask l1 xs)) 
                   | (x == (Just Y)) = Nothing
                   | otherwise = (mList (Just N) (makeMask l1 xs))
    where
      h | p == [] = replicate (head l1) (Just Y)
        | otherwise = reverse ((Just N):(replicate (head l1) (Just Y))) 
       where 
         p = drop (head l1) (x:xs)
      t | p == [] = makeMask (tail l1) []
        | otherwise = makeMask (tail l1) (tail p) 
       where 
         p = drop (head l1) (x:xs)

-- | Соединяeт 2 маски. Маски должны быть одной длины!
maskAppend :: Maybe [Cell] -> Maybe [Cell] -> Maybe [Cell]
maskAppend Nothing m = m
maskAppend m Nothing = m
maskAppend (Just []) _ = Just []
maskAppend _ (Just []) = (Just [])
maskAppend (Just (x:xs)) (Just (y:ys)) = mList (cellMul x y) (maskAppend (Just xs) (Just ys))

-- | Аналог операции ":" для списка. 
mList :: Cell -> Maybe [Cell] -> Maybe [Cell]
mList _ Nothing = Nothing
mList x (Just l) = Just (x:l)

-- | "Перемножение" клеток. 
cellMul :: Cell -> Cell -> Cell
cellMul (Just Y) (Just Y) = Just Y
cellMul (Just N) (Just N) = Just N
cellMul _ _ = Nothing

-- | Проверка, можно ли оставить конец ряда пустым.
checkEmpty :: [Cell] -> Bool
checkEmpty [] = True
checkEmpty (Just Y : _ ) = False
checkEmpty (_ : xs) = checkEmpty xs

-- | Сборка маски из частей.
maskAdd :: [Cell] -> Maybe [Cell] -> Maybe [Cell]
maskAdd _ Nothing = Nothing
maskAdd l1 (Just l2) = Just (l1 ++ l2)

-- | Проверка, может ли группа закрашенных клеток оказаться в начале
-- рассматриваемой части строки.
canPlace :: Int -> [Cell] -> Bool
canPlace 0 [] = True
canPlace _ [] = False
canPlace 0 (Just Y : _) = False
canPlace 0 _ = True
canPlace _ (Just N : _) = False
canPlace n (_ : xs) = canPlace (n-1) xs 

-- | Получить ряд.
getRow :: Board -> Int -> Line
getRow brd 0 = (head ((horizontal brd)), head (field brd))
getRow brd n | (length (horizontal brd) == 0) = ([],[])
             | otherwise = getRow (brd { horizontal = tail (horizontal brd)
                                      , field = tail (field brd) }) (n-1)

-- | Получить столбец.
getCol :: Board -> Int -> Line
getCol brd 0 = (reverse (head (vertical brd)), getFirst (field brd))
getCol brd n | (length (vertical brd) == 0) = ([],[])
             | otherwise = getCol (brd { vertical = tail (vertical brd)
                                      , field = getTail (field brd) }) (n-1)

-- | Список из первых элементов всех списков.                                      
getFirst :: [[a]] -> [a]
getFirst [[]] = []
getFirst l | length l == 1 = (head (head l) : [])
           | otherwise = head (head l) : getFirst (tail l)

-- | Список из всех списков без их первых элементов.       
getTail :: [[a]] -> [[a]]
getTail [[]] = [[]]
getTail l | length l == 1 = (tail (head l) : [])
          | otherwise = tail (head l) : getTail (tail l)
          
-- | Добавить столбец, то есть первый элемент в каждый список.
putFirst :: [a] -> [[a]] -> [[a]]
putFirst [] f = f
putFirst (x:xs) [[]] = (x:[]) : (putFirst xs [[]])  -- такого быть не может вообще-то
putFirst (x:xs) f = (x:(head f)) : (putFirst xs (tail f))  
