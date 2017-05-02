module DrawLogic where

import Graphics.Gloss.Interface.Pure.Game
import GraphicInterface
import Types
import SolverInterface
import Data.Time.Clock
import Data.Time.LocalTime
import Debug.Trace


drawField :: Board -> IO()
drawField brd = do
  play (display brd) bgColor fps (initGame brd) drawGame handleGame updateGame
     where
        display board = InWindow "Японские сканворды" ((screenWidth board + 11*cellSize), (screenHeight board)) (100, 100)
        bgColor = white       -- цвет фона
        fps = 60              -- кол-во кадров в секунду
        initGame board = board   
        
-- | Обработка событий.
handleGame :: Event -> Board -> Board
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) brd = (placeMark (mouseToCoord mouse brd) brd Y) {buttonPressed = True}
handleGame (EventKey (MouseButton LeftButton) Up _ _) brd = brd {buttonPressed = False}
handleGame (EventMotion mouse) brd = placeMarkY (mouseToCoord mouse brd) (buttonPressed brd) brd
handleGame (EventKey (MouseButton RightButton) Down _ mouse) brd = placeMark (mouseToCoord mouse brd) brd N
handleGame (EventKey (SpecialKey KeySpace) Down _ _) brd = startSolver brd
handleGame _ brd = brd


-- | Закрасить клетку (реакция на нажатие кнопок мыши).
placeMark :: (Int, Int) -> Board -> Mark -> Board
placeMark (i, j) brd m =
  case modifyArr j (modifyList i place) (field brd) of
      Nothing -> (checkOnBut (i, j) brd) -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> brd { field  = newBoard }
  where
    place Nothing = Just m
    place _  = Nothing
    
checkOnBut :: (Int, Int) -> Board -> Board
checkOnBut (i, j) brd | (trace ((show sw) ++ "   " ++ (show sh) ++ "   " ++ (show i) ++ "   " ++ (show j))((j>= sh-5) && (j<=sh-2) && (i<=sw+5) && (i>=sw + 2))) = startSolver brd
                      | otherwise = brd
  where 
    sw = fromIntegral(fieldWidth brd)
    sh = fromIntegral((screenSpace brd) + (fieldHeight brd))
    
-- | Закрасить клетку (реакция на движение кнопки мыши).
placeMarkY :: (Int, Int) -> Bool -> Board -> Board
placeMarkY _ False brd = brd
placeMarkY (i, j) True brd =
  case modifyArr j (modifyList i place) (field brd) of
      Nothing -> brd -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> brd { field  = newBoard }
  where
    place _ = Just Y


-- | Применить преобразование к элементу списка списков с заданным индексом.
modifyArr :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modifyArr _ _ []     = Nothing
modifyArr 0 f (x:xs)   = case f x of
  Nothing -> Nothing
  Just y -> Just (y : xs)
modifyArr i f (x:xs) = case modifyArr (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)
  
-- | Применить преобразование к элементу списка с заданным индексом.
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
          
-- | Обновление игрового поля.
-- При решении головоломки все изменения происходят только по действиям игрока.
updateGame :: Float -> Board -> Board
updateGame _ = id
