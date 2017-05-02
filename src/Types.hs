module Types where

import Data.Time.Clock
import Data.Time.LocalTime


-- | Основная структура данных - игровое поле.
data Board = Board 
    { field :: [[Cell]] -- игровые клетки
    , horizontal :: [[Int]] -- цифры слева от рядов
    , vertical :: [[Int]] -- цифры сверху от столбцов
    , fieldWidth :: Int -- ширина игрового поля в клетках
    , fieldHeight :: Int -- высота игрового поля в клетках
    , buttonPressed :: Bool -- зажата ли левая кнопка мыши
    , difficulty :: Maybe Int --сложность (можно определить с помощью авторешателя)
  --  , startTime :: TimeOfDay -- время начала решения
    }

-- | Фишки игроков.
data Mark = Y | N
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

