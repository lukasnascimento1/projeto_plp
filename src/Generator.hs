module Generator where

import Board (insertCharOnBoard, Board, emptyBoard, getRowFromBoard, validateCoordenates)
import System.Random (randomRIO)


--pegar os elementos de uma coluna ignorando as divisórias
--permite pegar uma coluna so com pipes (vou ajeitar depois)
getColumn :: Int -> Board -> [Char]
getColumn column board = [board !! r !! column | r <- [0 .. 10], r/=3, r /= 7]


--função pra auxiliar no getBox
findStartOfTheBox :: Int -> Int
findStartOfTheBox n
    |n >= 0 && n <= 2 = 0 --ta nas caixas do canto esquerdo
    |n >= 4 && n <= 6 = 4 --ta nas caixas do meio 
    |n >= 8 && n <= 10 = 8 --ta nas caixas da direita
    |otherwise = n 


--pegar as "caixas" 3x3
getBox :: Int -> Int -> Board -> [Char]
getBox row column board =
    let
        startRow = findStartOfTheBox row 
        startColumn = findStartOfTheBox column 
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
findEmptyPositions board = [(r, c) | r <- [0 .. 10], c <- [0 .. 10], validateCoordenates r c, (board !! r !! c) == 'x']
   

--solve recursivo pra fazer o jogo e depois a função de gerar poder remover os números de acordo com a dificuldade
solve :: Board -> [Char] -> [Board]
solve board nums =
    let emptyPositions = findEmptyPositions board
    in 
        if null emptyPositions then [board] --ja ta resolvido 
        else
            let (r, c) = head emptyPositions 
            in [result | n <- nums, isPositionValid n r c board, 
            Right newBoard <- [insertCharOnBoard n r c board], 
            result <- solve newBoard nums]


--função pra poder gerar a aleatoriedade
shuffleNumbers :: [Char] -> IO[Char]
shuffleNumbers [] = return []
shuffleNumbers l = do
    i <- randomRIO(0, length l - 1)
    let num = l !! i
    let restOfList = take i l ++ drop (i + 1) l 
    rest <- shuffleNumbers restOfList
    return (num : rest) 


--gerando o tabuleiro cheio pra remover os números depois
generateFilledBoard :: IO Board
generateFilledBoard = do
    shuffledNumbers <- shuffleNumbers ['1' .. '9']
    let solution = solve emptyBoard shuffledNumbers
    return (head solution) 




