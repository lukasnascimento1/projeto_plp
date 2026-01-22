module Generator where

import Board (insertCharOnBoard, Board, emptyBoard, getRowFromBoard, validateCoordenates, deleteCharFromBoard)
import System.Random (randomRIO)

--pegar os elementos de uma coluna ignorando as divisórias
--permite pegar uma coluna so com pipes (vou ajeitar depois)
getColumn :: Int -> Board -> [Char]
getColumn column board = [board !! r !! column | r <- [0 .. 8]]

--pegar as "caixas" 3x3
getBox :: Int -> Int -> Board -> [Char]
getBox row column board =
    let
        startRow = (row `div` 3) * 3
        startColumn = (column `div` 3) * 3
    in
        [board !! (startRow + r) !! (startColumn + c) | r <-  [0..2], c <- [0..2]]

--validando as 'jogadas' na hora de gerar o tabuleiro
isPositionValid :: Char -> Int -> Int -> Board -> Bool
isPositionValid n row column board =
    let
        numberNotInRow = not (n `elem` (getRowFromBoard row board))
        numberNotInColumn = not (n `elem` (getColumn column board))
        numberNotInBox = not (n `elem` (getBox row column board))
    in
        numberNotInRow && numberNotInColumn && numberNotInBox

--achar posicoes vazias
findEmptyPositions :: Board -> [(Int, Int)]
findEmptyPositions board = [(r, c) | r <- [0..8], c <- [0..8], (board !! r !! c) == '.']

--solve pra fazer o jogo e depois a função de gerar poder remover os números de acordo com a dificuldade
solve :: Board -> IO[Board]
solve board = do
    let emptyPositions = findEmptyPositions board
    if null emptyPositions then return [board] --ja ta resolvido
    else do
        let (row, column) = head emptyPositions
        nums <- shuffleList ['1' .. '9']
        auxTryNumbers board row column nums

auxTryNumbers :: Board -> Int -> Int -> [Char] -> IO[Board]
auxTryNumbers _ _ _ [] = return []
auxTryNumbers board row column (x:xs) = do
    if isPositionValid x row column board
        then do
            let(Right newBoard) = insertCharOnBoard x row column board
            result <- solve newBoard
            if null result
                then auxTryNumbers board row column xs
                else return result
        else auxTryNumbers board row column xs

--função pra poder gerar a aleatoriedade
shuffleList :: [a] -> IO[a]
shuffleList [] = return []
shuffleList l = do
    i <- randomRIO(0, length l - 1)
    let num = l !! i
    let restOfList = take i l ++ drop (i + 1) l
    rest <- shuffleList restOfList
    return (num : rest)

--gerando o tabuleiro cheio pra remover os números depois
generateFilledBoard :: IO Board
generateFilledBoard = do
    solution <- solve emptyBoard
    return (head solution)

-- remove a quantidade de números passada como parâmetro
removeNumbersToGenerateGame :: Int -> Board -> IO Board
removeNumbersToGenerateGame 0 board = return board
removeNumbersToGenerateGame n board = do
    row <- randomRIO(0, 8)
    column <- randomRIO(0, 8)
    if (board !! row !! column) /= '.'
        then do
            let (Right newBoard) = deleteCharFromBoard row column board
            removeNumbersToGenerateGame (n - 1) newBoard
        else
            removeNumbersToGenerateGame n board

generateEasy :: IO Board
generateEasy = do
    fullGame <- generateFilledBoard
    removeNumbersToGenerateGame 35 fullGame

generateHard :: IO Board
generateHard = do
    fullGame <- generateFilledBoard
    removeNumbersToGenerateGame 60 fullGame
