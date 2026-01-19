{- Construir o tabuleiro do Sudoku
    é um tabuleiro 9x9 porém tem também o espaço para os pipes
    que separam os numeros, logo o tabuleiro se torna uma matriz 11x11
    Funções de Inserir e Remover elementos no tabuleiro
-}

module Board where

    type Board = [[Char]] -- = [String]

    -- Forma de expressar uma excecao
    data BoardError
        = InvalidCoordinates
        deriving (Show, Eq)

    textColorWhiteWeak = "\ESC[2;37m"
    textColorGreenWeak = "\ESC[2;32m"
    textColorBlueWeak = "\ESC[2;34m"
    textColorCyanWeak = "\ESC[2;36m"
    textColorGreen = "\ESC[92m"
    textBold = "\ESC[1m"
    resetColor = "\ESC[0m"

    coordenadasX = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']
    coordenadasY = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    linhaVaziaTabuleiro = ['x', 'x', 'x'] ++ ['|'] ++ ['x', 'x', 'x'] ++ ['|'] ++ ['x', 'x', 'x']
    -- linhaVaziaTabuleiro = "\ESC[32m" ++ 
    --                         ['x', 'x', 'x'] ++ "\ESC[2;37m" ++ ['|'] ++ "\ESC[0m" ++
    --                         ['x', 'x', 'x'] ++ "\ESC[2;37m" ++ ['|'] ++ "\ESC[0m" ++
    --                         ['x', 'x', 'x'] ++
    --                         "\ESC[0m"
    
    linhaPipe = replicate 11 '-'

    -- inicia o valor de tabuleiro como um tabuleiro vazio
    tabuleiro = emptyBoard

    -- Constrói um tabuleiro vazio
    emptyBoard :: Board
    emptyBoard = (replicate 3 linhaVaziaTabuleiro) ++ [linhaPipe]
                ++ (replicate 3 linhaVaziaTabuleiro) ++ [linhaPipe]
                ++ (replicate 3 linhaVaziaTabuleiro)
    
    -- Dá cor às divisorias do tabuleiro
    coloredBoard :: String -> String
    coloredBoard [] = []
    coloredBoard (c:cs)
        | c == 'x' = textColorGreen ++ "x" ++ resetColor
        | c == '|' = textColorWhiteWeak ++ "|" ++ resetColor
        | c == '-' = "\ESC[34m" ++ "-" ++ resetColor
        | otherwise = c : coloredBoard cs


    -- indices proibidos na matriz: 3 e 7 pois são as divisorias

    -- Adiciona um elemento do tipo Char na linha do tabuleiro
    insertCharOnRow :: Char -> Int -> [Char] -> [Char]
    insertCharOnRow num ind row
        | ind < 0 || ind >= length row = row
        | otherwise = take ind row ++ [num] ++ drop (ind + 1) row


    -- Insere um elemento do tipo Char no tabuleiro
    insertCharOnBoard :: Char -> Int -> Int -> Board -> Either BoardError Board
    insertCharOnBoard num row col board
        | not (validateCoordenates row col) = Left InvalidCoordinates
        | otherwise =
            Right $ -- esse $ é uma abstracao para parenteses: a * (b + c) = a * $ b + c
                take row board
                ++ [insertCharOnRow num col (getRowFromBoard row board)]
                ++ drop (row + 1) board

    -- Retorna a linha da matriz a partir do indice e do tabuleiro nos parametos
    getRowFromBoard :: Int -> Board -> [Char]
    getRowFromBoard row board = board !! row

    -- Deleta um elemento do tabuleiro a partir das coordenadas dadas
    deleteCharFromBoard :: Int -> Int -> Board -> Either BoardError Board
    deleteCharFromBoard row col board =
        insertCharOnBoard 'x' row col board

    -- Valida as coordenadas da jogada
    -- Os parametros das funcoes são indices
    -- As linhas e colunas 4 e 8 da matriz (indices 3 e 7) são as divisorias do tabuleiro
    validateCoordenates :: Int -> Int -> Bool
    validateCoordenates x y
        | not (inRange x && inRange y) = False
        | x == y && (validInd x == False) = False
        | x /= y && ((validInd x && validInd y) == False) = False
        | otherwise = True
        where
            inRange i = i >= 0 && i <= 10
            validInd i = (i /= 3) && (i /= 7)


    -- Imprime o tabuleiro formatado na tela
    printBoard :: Board -> Board -> IO ()
    printBoard initialBoard currentBoard = do
        putStrLn cabeçalho  -- imprime a linha de cabeçalho com os numeros das colunas
        putStrLn "" -- espaçamento entre cabeçalho e tabuleiro

        let tabuleiroComRodape = adicionaRotulos coordenadasX initialBoard currentBoard ++ [linhaFinal]
        putStr (unlines tabuleiroComRodape)  -- imprime o tabuleiro com os rotulos das linhas
        where
            espaçar s = unwords (map (:[]) s)
            -- Cria o cabeçalho com os números de 1 a 9 espaçados e separados por espaco a cada 3 numeros
            cabeçalho = "\ESC[1;35m    1 2 3   4 5 6   7 8 9\ESC[0m"
            -- cabeçalho = "    1 2 3   4 5 6   7 8 9"
            separadorLargo = replicate 21 '-' --"\ESC[2;36m-\ESC[0"
            linhaFinal = textColorWhiteWeak ++ "    " ++ separadorLargo ++ resetColor
            -- Função recursiva que associa cada letra a sua respectiva linha
            adicionaRotulos :: [Char] -> Board -> Board -> [String]
            adicionaRotulos _ [] [] = []  -- caso base: quando não tem mais linhas, retorna lista vazia
            adicionaRotulos rotulos (linhaIni:restoIni) (linhaCur:restoCur)
                -- Se a linha tem '-', é uma linha separadora: adiciona espaçamento e não consome letra
                | '-' `elem` linhaCur = (textColorWhiteWeak ++ "    " ++ separadorLargo ++ resetColor) : adicionaRotulos rotulos restoIni restoCur
                -- Se não é separadora: adiciona a primeira letra + espaço + conteúdo da linha, depois continua com letras restantes
                | otherwise = 
                    -- let linhaFormatada = espaçar linha
                    -- let linhaFormatada = unwords (map (colorChar) linha)
                    let linhaFormatada = unwords (zipWith colorChar linhaIni linhaCur)
                    in (head rotulos : "   " ++ linhaFormatada) : adicionaRotulos (tail rotulos) restoIni restoCur
                    where
                        colorChar :: Char -> Char -> String
                        colorChar initC curC
                            | curC `elem` ['1'..'9'] && initC /= 'x'
                                = textColorCyanWeak ++ [curC] ++ resetColor      -- número original
                            | curC `elem` ['1'..'9']
                                = textBold ++ textColorGreen ++ [curC] ++ resetColor      -- número do usuário
                            | curC == 'x'
                                = textColorGreenWeak ++ "x" ++ resetColor
                            | curC == '|' || curC == '-'
                                = textColorWhiteWeak ++ [curC] ++ resetColor
                            | otherwise
                                = [curC]

