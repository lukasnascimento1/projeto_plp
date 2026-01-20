{-
    Tabuleiro do Sudoku é uma matriz 9x9
    Funções de Inserir e Remover elementos no tabuleiro
-}

module Board where

    type Board = [[Char]] -- = [String]

    -- Forma de expressar uma excecao
    data BoardError = InvalidCoordinates deriving (Show, Eq)

    -- Constrói um tabuleiro vazio
    emptyBoard :: Board
    emptyBoard = replicate 9 (replicate 9 'x')

    -- Valida as coordenadas da jogada
    -- Os parametros das funcoes são indices
    validateCoordenates :: Int -> Int -> Bool
    validateCoordenates x y = x >= 0 && x < 9 && y >= 0 && y < 9

    -- Adiciona um elemento do tipo Char na linha do tabuleiro
    insertCharOnRow :: Char -> Int -> [Char] -> [Char]
    insertCharOnRow num ind row = take ind row ++ [num] ++ drop (ind + 1) row

    -- Insere um elemento do tipo Char no tabuleiro
    insertCharOnBoard :: Char -> Int -> Int -> Board -> Either BoardError Board
    insertCharOnBoard num row col board
        | not (validateCoordenates row col) = Left InvalidCoordinates
        | otherwise = Right $
                take row board
                ++ [insertCharOnRow num col (board !! row)]
                ++ drop (row + 1) board

    -- Retorna a linha da matriz a partir do indice e do tabuleiro nos parametos
    getRowFromBoard :: Int -> Board -> [Char]
    getRowFromBoard row board = board !! row

    -- Deleta um elemento do tabuleiro a partir das coordenadas dadas
    deleteCharFromBoard :: Int -> Int -> Board -> Either BoardError Board
    deleteCharFromBoard row col board = insertCharOnBoard 'x' row col board
