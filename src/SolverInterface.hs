module SolverInterface where

import AutoSolver
import Types

-- | Запуск автоматического решателя.
startSolver :: Board -> Board
startSolver brd = autoSolve (SolveBoard { playBoard = brd
                                        , changed = False
                                        , solvingSteps = 1
                                        })
-- | Определение сложности головоломки.
-- Если головоломка нерешаема, функция вернет Nothing.
getDifficulty :: Board -> Maybe Int
getDifficulty brd = (difficulty (startSolver brd))


