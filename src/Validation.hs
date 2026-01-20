{-
    Módulo responsável pela lógica de validação da solução
-}

module Validation where

import Board (Board, getRowFromBoard)
import Generator (getColumn, getBox)
import Data.List (sort)

-- função auxiliar do módulo de validação
-- apenas verifica se uma determinada sequência é igual à sequência válida do Sudoku (1 a 9, sem repetições)
isSequenceValid :: [Char] -> Bool
isSequenceValid sequence = sort (filter (`elem` ['1'..'9']) sequence ) == "123456789"

-- verifica se todas as linhas formam uma sequência válida
areAllRowsValid :: Board -> Bool
areAllRowsValid board = all (\r -> isSequenceValid (getRowFromBoard r board)) [0..8]

-- verifica se todas as colunas formam uma sequência válida
areAllColumnsValid :: Board -> Bool
areAllColumnsValid board = all (\c -> isSequenceValid (getColumn c board)) [0..8]

-- verifica se todas as submatrizes 3x3 formam uma sequência válida
areAllBoxesValid :: Board -> Bool
areAllBoxesValid board = all (\(r, c) -> isSequenceValid (getBox r c board)) startingPoints
    where startingPoints = [ (r, c) | r <- [0, 3, 6], c <- [0, 3, 6] ]

-- verifica se a solução cumpre os requisitos: todas as linhas, colunas e submatrizes formam sequências válidas
isSolutionValid :: Board -> Bool
isSolutionValid board = areAllRowsValid board && areAllColumnsValid board && areAllBoxesValid board
