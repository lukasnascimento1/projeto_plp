module Generator where

import Board
import Board (insertCharOnBoard)

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






