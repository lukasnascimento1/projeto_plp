{- Construir o tabuleiro do Sudoku
    é um tabuleiro 9x9 porém tem também o espaço para os pipes 
    que separam os numeros
-}

module Board where

    type Board = [[Char]] -- = [String]

    data BoardError
        = InvalidCoordinates
        deriving (Show, Eq)
    
    coordenadasX = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    coordenadasY = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    linhaVaziaTabuleiro = ['x', 'x', 'x'] ++ ['|'] ++ ['x', 'x', 'x'] ++ ['|'] ++ ['x', 'x', 'x']
    linhaPipe = replicate 11 '-'
    
    tabuleiro = emptyBoard

    
    {- emptyBoard retorna uma matriz 11x11 somente com 'x's e as barras de divisao
    usar take e drop para operar os elementos da matriz
    take :: Int -> [a] -> [a]
    drop :: Int -> [a] -> [a]
     essas funcoes descartam os n ultimos/primeiros elementos e retorna o resto

    -}
    emptyBoard :: Board 
    emptyBoard = (replicate 3 linhaVaziaTabuleiro) ++ [linhaPipe]
                ++ (replicate 3 linhaVaziaTabuleiro) ++ [linhaPipe]
                ++ (replicate 3 linhaVaziaTabuleiro)
    
    -- insertNumOnBoard :: Int -> Int -> Int -> [[Char]] -> [[Char]]
    -- insertNumOnBoard number row column board = 
    

    -- indices proibidos na matriz: 3 e 7 pois são as divisorias

    insertCharOnRow :: Char -> Int -> [Char] -> [Char]
    insertCharOnRow num ind row = 
        take ind row ++ [num] ++ drop (ind + 1) row


    insertCharOnBoard :: Char -> Int -> Int -> Board -> Either BoardError Board
    insertCharOnBoard num row col board
        | not (validateCoordenates row col) = Left InvalidCoordinates
        | otherwise =
            Right $ -- esse $ é uma abstracao para parenteses: a * (b + c) = a * $ b + c
                take row board
                ++ [insertCharOnRow num col (getRowFromBoard row board)]
                ++ drop (row + 1) board

    
    getRowFromBoard :: Int -> Board -> [Char]
    getRowFromBoard row board = board !! row


    validateCoordenates :: Int -> Int -> Bool
    validateCoordenates x y
        | not (inRange x && inRange y) = False
        | x == y && (validInd x == False) = False
        | x /= y && ((validInd x && validInd y) == False) = False
        | otherwise = True
        where
            inRange i = i >= 0 && i <= 10
            validInd i = (i /= 3) && (i /= 7)


    printBoard :: Board -> IO ()
    printBoard = putStrLn . unlines

    {-
    Noting:
        Entao para inserir ou remover um numero no tabuleiro será preciso 
        usar take e drop
        Essas funcoes irão montar um novo [Char]
    -}