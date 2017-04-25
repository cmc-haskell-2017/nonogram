module AutoSolver where

import Types
import Debug.Trace

type Line = ([Int], [Cell])

data SolveBoard = SolveBoard
    { playBoard :: Board
    , changed :: Bool
    ,solvingSteps :: Int
    }

-- | Функция решателя, работающая, пока все клетки не будут заполнены и пока её раота изменяет хоть что-то.
autoSolve :: SolveBoard -> Board
autoSolve brd | ((changed new_brd) == False) || (isSolved (field (playBoard new_brd))) = countDifficulty new_brd
              | otherwise = autoSolve new_brd { solvingSteps = (solvingSteps new_brd) + 1
                                               , changed = False
                                               }
    where
      new_brd = checkCols 0 (checkRows 0 brd)

countDifficulty :: SolveBoard -> Board
countDifficulty brd | (difficulty (playBoard brd)) /= Nothing = (playBoard brd) 
                    | (changed brd) == False = (playBoard brd){difficulty = Nothing}
                    | otherwise = (playBoard brd){difficulty = Just (solvingSteps brd)}

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

-- | Обработка всех строк, начиная с заданного номера.
checkRows :: Int -> SolveBoard -> SolveBoard
checkRows n brd | (n < (fieldHeight (playBoard brd))) = checkRows (n+1) (checkRow n brd)
                | otherwise = brd
                
-- | Обработка строки с заданным номером.
checkRow :: Int -> SolveBoard -> SolveBoard
checkRow n brd = placeRow brd (makeMask ns ls) n
    where
      (ns, ls) = getRow (playBoard brd) n

-- | Обработка всех столбцов, начиная с заданного номера.
checkCols :: Int -> SolveBoard -> SolveBoard
checkCols n brd | (n < (fieldWidth (playBoard brd))) = checkCols (n+1) (checkCol n brd)
                | otherwise = brd

-- | Обработка стобца с заданным номером.
checkCol :: Int -> SolveBoard -> SolveBoard
checkCol n brd = placeCol brd (makeMask ns ls) n
    where
      (ns, ls) = getCol fld{vertical = reverse (vertical fld)} n
        where
          fld = playBoard brd

-- | Изменение строки с заданным номером, если необходимо.
placeRow :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeRow brd m n | (eqLine cs m) = brd
                 | otherwise = brd { playBoard = fld{field = putRow (field fld) m n}
                                   , changed = True}
    where
      (_, cs) = getRow fld n
      fld = playBoard brd

-- | Добавление измененной строки к полю.
putRow :: [[Cell]] -> Maybe [Cell] -> Int -> [[Cell]]
putRow f Nothing _ = f
putRow f (Just l) 0 = l : (tail f)
putRow f m n = (head f) : (putRow (tail f) m (n-1))                   

-- | Изменение столбца с заданным номером, если необходимо.
placeCol :: SolveBoard -> Maybe [Cell] -> Int -> SolveBoard
placeCol brd m n | (eqLine cs m) = brd
                 | otherwise = brd { playBoard = fld{field = putCol (field fld) m n}
                                   , changed = True}
    where
      (_, cs) = getCol fld n
      fld = playBoard brd
                 
-- | Добавление измененного столбца к полю.
putCol :: [[Cell]] -> Maybe [Cell] -> Int -> [[Cell]]
putCol f Nothing _ = f
putCol [[]] _ _ = [[]]
putCol f (Just l) 0 = putFirst l (getTail f)
putCol f m n = putFirst (getFirst f) (putCol (getTail f) m (n-1))

-- | Проверка на равенство маски и ряда.
eqLine :: [Cell] -> Maybe [Cell] -> Bool
eqLine _ Nothing = True
eqLine [] (Just []) = True
eqLine (x:[]) (Just (y:_)) = (eqCell x y)
eqLine (x:xs) (Just (y:ys)) | (eqCell x y) = eqLine xs (Just ys)
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
