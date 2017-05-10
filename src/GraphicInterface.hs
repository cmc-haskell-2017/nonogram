module GraphicInterface where
-- Модуль, отвечающий за отрисовку.
import Graphics.Gloss.Interface.Pure.Game
import Types
import Debug.Trace

{-
defaultGame = Board
    { fieldWidth  = 20  
    , fieldHeight = 10
    , field = replicate (fieldHeight initGame) (replicate (fieldWidth initGame) Nothing)
    , horizontal = ([1, 2, 3, 4] : (replicate ((fieldHeight initGame)-1) [6, 7, 8] ))
    , vertical = replicate (fieldWidth initGame) [1] 
    }
-}

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 20

-- | Отступ от края.
screenSpace :: Board -> Int 
screenSpace brd =  max (maxLngt (horizontal brd)) (maxLngt (vertical brd))

-- | Ширина экрана в пикселях.
screenWidth :: Board -> Int
screenWidth brd = cellSize * (fieldWidth brd + screenSpace brd)

-- | Высота экрана в пикселях.
screenHeight :: Board -> Int 
screenHeight brd = cellSize * (fieldHeight brd + screenSpace brd)

-- | Максимальная длина. 
maxLngt :: [[a]] -> Int
maxLngt lst = maximum (map length lst)

drawGame :: Board -> Picture
drawGame board = translate (-w) (-h) (scale c c (pictures
  [ drawGrid board
  , drawBoard board
  , drawNums board
  , drawMenu board
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral (screenWidth board)  / 2
    h = fromIntegral (screenHeight board) / 2

drawMenu :: Board -> Picture
drawMenu board = color black (pictures 
  [ line [(sw+2, sh-2), (sw+5, sh-2)]
  , line [(sw+5, sh-2), (sw+5, sh-5)]
  , line [(sw+5, sh-5), (sw+2, sh-5)]
  , line [(sw+2, sh-5), (sw+2, sh-2)]
  , translate (0.3 + sw+2) (0.2 + sh-5 ) (pictures [scale 0.004 0.004 $ color black $ text $ "AutoSolver"] )
  ])
  where
    sw = fromIntegral((screenSpace board) + (fieldWidth board))
    sh = fromIntegral((screenSpace board) + (fieldHeight board))
    


-- | Сетка игрового поля.
drawGrid :: Board -> Picture
drawGrid board = color black (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(dx, j), (n, j)]) [0..m - ss]
    vs = map (\i -> line [(i, 0), (i, m - dy)]) [ss..n]

    ss = fromIntegral(screenSpace board) 
    dx = fromIntegral(screenSpace board - length(vertical board))
    dy = fromIntegral(screenSpace board - length(horizontal board))
    n = fromIntegral (fieldWidth board + (screenSpace board))
    m = fromIntegral (fieldHeight board + screenSpace board)

-- | Нарисовать цифры по краям
drawNums :: Board -> Picture
drawNums board = pictures (mappend drawHor drawVert)
  where
    drawHor = foldMap drawHRow (zip [0..] (reverse (horizontal board)))
    drawVert = foldMap drawVRow (zip [0..] (reverse (vertical board)))
    drawHRow (j, row) = map drawHN (zip [0..] (reverse row))
     where
        drawHN (i, num) = translate (0.3 + fromIntegral (screenSpace board) - i - 1) (0.2 + fromIntegral (fieldHeight board) - j - 1)
          (pictures [scale 0.004 0.004 $ color black $ text $ show num] )
    drawVRow (i, row) = map drawVN (zip [0..] (reverse row))
      where
        drawVN (j, num) = translate (0.3 + fromIntegral (screenSpace board) + i) (0.2 + fromIntegral (fieldHeight board) + j)
          (pictures [scale 0.004 0.004 $ color black $ text $ show num] )


-- | Нарисовать пометки на игровом поле.
drawBoard :: Board -> Picture
drawBoard board = pictures drawCells
  where
    drawCells = foldMap drawRow (zip [0..] (field board))
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i + fromIntegral (screenSpace board)) (0.5 + j)
          (drawCell cell)

-- | Отметить клетку поля (если нужно).
drawCell :: Cell -> Picture
drawCell Nothing = blank
drawCell (Just mark)
  = color markColor (drawMark mark)
  where
    markColor
      | mark == Y = black
      | otherwise = black


-- | Нарисовать отметку в клетке.
drawMark :: Mark -> Picture
drawMark Y = drawY
drawMark N = drawN

-- | Нарисовать «закрашенная клетка».
drawY :: Picture
drawY = polygon [(-0.46,  -0.45), (-0.46,  0.46), ( 0.45, 0.46), ( 0.45, -0.45)]

-- | Нарисовать «пустая клетка».
drawN :: Picture
drawN = thickCircle (0.0015 * fromIntegral cellSize) (0.003 * fromIntegral cellSize)
