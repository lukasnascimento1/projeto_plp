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


    -- indices proibidos na matriz: 3 e 7 pois são as divisorias

    -- Adiciona um elemento do tipo Char na linha do tabuleiro
    insertCharOnRow :: Char -> Int -> [Char] -> [Char]
    insertCharOnRow num ind row =
        take ind row ++ [num] ++ drop (ind + 1) row


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
    printBoard :: Board -> IO ()
    printBoard board = do
        putStrLn cabeçalho  -- imprime a linha de cabeçalho com os numeros das colunas
        putStrLn (unlines linhasComRotulos)  -- imprime o tabuleiro com os rotulos das linhas
        where
            -- Cria o cabeçalho com os números de 1 a 9 espaçados e separados por espaco a cada 3 numeros
            cabeçalho = "  " ++ ['1', '2', '3', ' ', '4', '5', '6', ' ', '7', '8', '9']

            -- Adiciona os rótulos de letras (A-I) às linhas do tabuleiro
            linhasComRotulos = adicionaRotulos coordenadasX board

            -- Função recursiva que associa cada letra a sua respectiva linha
            adicionaRotulos :: [Char] -> Board -> [String]
            adicionaRotulos _ [] = []  -- caso base: quando não tem mais linhas, retorna lista vazia
            adicionaRotulos rotulos (linha:resto)
                -- Se a linha tem '-', é uma linha separadora: adiciona espaçamento e não consome letra
                | '-' `elem` linha = ("  " ++ linha) : adicionaRotulos rotulos resto
                -- Se não é separadora: adiciona a primeira letra + espaço + conteúdo da linha, depois continua com letras restantes
                | otherwise = (head rotulos : ' ' : linha) : adicionaRotulos (tail rotulos) resto

    {-
    Noting:
        Entao para inserir ou remover um numero no tabuleiro será preciso
        usar take e drop
        Essas funcoes irão montar um novo [Char]
    -}