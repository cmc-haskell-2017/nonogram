import Graphics.Gloss.Interface.Pure.Game

data Board = Board 
    { field :: [[Maybe Mark]] -- игровые клетки
    , horizontal :: [[Int]] -- цифры слева от рядов
    , vertical :: [[Int]] -- цифры сверху от столбцов
    , fieldWidth :: Int -- ширина игрового поля в клетках
    , fieldHeight :: Int -- высота игрового поля в клетках
    , screenSpace :: Int -- отступ от края
    }

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 35

-- | Фишки игроков.
data Mark = Y | N
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

main :: IO()
main = do 
    drawField
    
drawField :: IO()
drawField = do
  let brd = initGame
  play (display brd) bgColor fps initGame drawGame handleGame updateGame
     where
        display brd = InWindow "Японские сканворды" ((screenWidth brd), (screenHeight brd)) (100, 100)
        bgColor = white   -- цвет фона
        fps = 60 -- кол-во кадров в секунду

initGame = Board
    { fieldWidth  = 20 -- 
    , fieldHeight = 10
    , screenSpace =  max (maxLngt (horizontal initGame)) (maxLngt (vertical initGame))
    , field = replicate (fieldHeight initGame) (replicate (fieldWidth initGame) Nothing)
    , horizontal = ([1, 2, 3, 4] : (replicate ((fieldHeight initGame)-1) [6, 7, 8] ))--                                            сделать загрузку
    , vertical = replicate (fieldWidth initGame) [1]         --                                               из файла
    }

-- | Ширина экрана в пикселях.
screenWidth :: Board -> Int
screenWidth brd = cellSize * ((fieldWidth brd) + (screenSpace brd))

-- | Высота экрана в пикселях.
screenHeight :: Board -> Int 
screenHeight brd = cellSize * ((fieldHeight brd) + (screenSpace brd))



drawGame :: Board -> Picture
drawGame board = translate (-w) (-h) (scale c c (pictures
  [ drawGrid board
  , drawBoard False board
  , drawNums board
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral (screenWidth board)  / 2
    h = fromIntegral (screenHeight board) / 2

-- | Сетка игрового поля.
drawGrid :: Board -> Picture
drawGrid board = color black (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(dx, j), (n, j)]) [0..m - ss]
    vs = map (\i -> line [(i, 0), (i, m - dy)]) [ss..n]

    ss = fromIntegral(screenSpace board)
    dx = fromIntegral((screenSpace board) - lngt(vertical board))
    dy = fromIntegral((screenSpace board) - lngt(horizontal board))
    n = (fromIntegral ((fieldWidth board) + (screenSpace board)))
    m = (fromIntegral ((fieldHeight board) + (screenSpace board)))

-- | Длина списка    
lngt :: [a] -> Int
lngt [] = 0
lngt (x:xs) = 1 + lngt xs

-- | Максимальная длина 
maxLngt :: [[a]] -> Int
maxLngt lst = foldl1 max (map lngt lst)

-- | Нарисовать цифры по краям
drawNums :: Board -> Picture
drawNums board = pictures (map pictures (drawHor ++ drawVert))
  where
    drawHor = map drawHRow (zip [0..] (horizontal board))
    drawVert = map drawVRow (zip [0..] (vertical board))
    drawHRow (j, row) = map drawHN (zip [0..] (reverse row))
     where
        drawHN (i, num) = translate (0.5 + (fromIntegral (screenSpace board)) - i -1) (0.2 + (fromIntegral (fieldHeight board)) - j -1)
          (pictures [scale 0.004 0.004 $ color black $ text $ show num] )
    drawVRow (i, row) = map drawVN (zip [0..] (reverse row))
      where
        drawVN (j, num) = translate (0.5 + (fromIntegral (screenSpace board)) + i) (0.2 + (fromIntegral (fieldHeight board)) + j)
          (pictures [scale 0.004 0.004 $ color black $ text $ show num] )

-- | Нарисовать фишки на игровом поле.
drawBoard :: Bool -> Board -> Picture
drawBoard win board = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] (field board))
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i + fromIntegral (screenSpace board)) (0.5 + j)
          (drawCell (estimate board) win cell)

-- | Нарисовать фишку в клетке поля (если она там есть).
drawCell :: (Int, Int) -> Bool -> Cell -> Picture
drawCell _ _ Nothing = blank
drawCell (x, o) win (Just mark)
  = color markColor (drawMark mark)
  where
    markColor
      | win = red
      | mark == Y = black
      | otherwise = black


-- | Нарисовать фишку.
drawMark :: Mark -> Picture
drawMark Y = drawY
drawMark N = drawN

-- | Нарисовать «закрашенная клетка».
drawY :: Picture
drawY = polygon [(-0.48,  -0.48), (-0.48,  0.48), ( 0.48, 0.48), ( 0.48, -0.48)]

-- | Нарисовать «пустая клетка».
drawN :: Picture
drawN = thickCircle (0.001 * (fromIntegral cellSize)) (0.002 * (fromIntegral cellSize))

-- | Оценить состояние игрового поля, а именно
-- вычислить сумму длин сегментов для крестиков и ноликов.
-- Сегменты длины 1 не учитываются при подсчёте.
estimate :: Board -> (Int, Int)
estimate _ = (0, 0)

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Board -> Board
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) brd = placeMarkY (mouseToCoord mouse brd) brd
handleGame (EventKey (MouseButton RightButton) Down _ mouse) brd = placeMarkN (mouseToCoord mouse brd) brd
handleGame _ brd = brd

-- | Поставить отметку.
placeMarkY :: (Int, Int) -> Board -> Board
placeMarkY (i, j) brd =
  case modifyArr j (modifyList i place) (field brd) of
      Nothing -> brd -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> brd
        { field  = newBoard
        , horizontal = (horizontal brd)
        , vertical = (vertical brd)
        , fieldWidth = (fieldWidth brd)
        , fieldHeight = (fieldHeight brd)
        , screenSpace = (screenSpace brd)
        }
  where
    place Nothing = Just Y
--    place (Just N) = Just N
    place _  = Nothing
    
-- | Поставить отметку.
placeMarkN :: (Int, Int) -> Board -> Board
--placeMarkN Nothing brd = brd
placeMarkN (i, j) brd =
  case modifyArr j (modifyList i place) (field brd) of
      Nothing -> brd -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> brd
        { field  = newBoard
        , horizontal = (horizontal brd)
        , vertical = (vertical brd)
        , fieldWidth = (fieldWidth brd)
        , fieldHeight = (fieldHeight brd)
        , screenSpace = (screenSpace brd)
        }
  where
    place Nothing = Just N
    place _ = Nothing
  --  place (Just Y) = Just Y


-- | Применить преобразование к элементу списка
-- с заданным индексом. Если преобразование не удалось — вернуть Nothing.
-- Иначе вернуть преобразованный список.
modifyArr :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modifyArr _ _ []     = Nothing
modifyArr 0 f (x:xs)   = case f x of
  Nothing -> Nothing
  Just y -> Just (y : xs)
modifyArr i f (x:xs) = case modifyArr (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

modifyList :: Int -> (a -> a) -> [a] -> Maybe [a]
modifyList _ _ []     = Nothing
modifyList 0 f (x:xs) = Just ((f x) : xs)
modifyList i f (x:xs) = case modifyList (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

-- | Получить координаты клетки под мышкой.
mouseToCoord :: Point -> Board -> (Int, Int)
mouseToCoord (x, y) brd = (i, j)
  where
    i = floor (x + fromIntegral (screenWidth brd)  / 2) `div` cellSize - (screenSpace brd)
    j = floor (y + fromIntegral (screenHeight brd) / 2) `div` cellSize
          
-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Board -> Board
updateGame _ = id

