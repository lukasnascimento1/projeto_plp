

{-
    Interface de interação do jogo
-}
module UI where

    import Text.Read (readMaybe)
    import Data.Char (digitToInt, isDigit, toLower)
    import Control.Concurrent (threadDelay)
    import qualified Board as B

    type Cell = String --[Char]

    tutorial = "[I] Inserir um número;"
            ++"\n[D] Deletar um número;"
            ++"\nAo selecionar essas ações você precisará inserir as coordenadas da jogada ('A1', 'D7', 'I9')"
            ++"\n[H] Help"
            ++"\n[R] Restart"

    menuInicial = "[A] Sobre o Sudoku\n[T] Tutorial\nQualquer outra tecla inicia o jogo\n"
    finalizou = "Yeah!!! Você finalizou este SUDOKU!"
    about = "Sudoku é um jogo de lógica em que se preenche uma grade 9×9 com números de 1 a 9, sem repetir valores em linhas, colunas e regiões 3×3."


    menu :: IO ()
    menu = do
        putStrLn "Pratique seu raciocínio lógico com: \n------>\tSUDOKU!\t<------"
        threadDelay 500000
        putStrLn ""
        putStrLn menuInicial
        action <- getLine
        firstAction action
        menuFinal
        

    menuFinal :: IO ()
    menuFinal = do
        putStrLn "Parabéns!!!\nVocê Finalizou um SUDOKU\nDeseja jogar novamente? y/n"
        r <- getLine
        case map toLower r of
            "y" -> menu
            _ -> return ()


    {-
    Seleciona a primeira ação a partir do Menu incial do jogo:
    A) About sudoku T) Tutorial do game X) Qualquer outra tecla inicia o jogo
    -}
    firstAction :: String -> IO ()
    firstAction action = do
        case map toLower action of
            "a" -> do
                putStrLn about -- descrição do sudoku
                menu
            "t" -> do
                putStrLn tutorial -- tutorial das funcoes 
                menu
            _-> startGame

    -- Inicia o modo de jogo
    startGame :: IO ()
    startGame = do
        putStrLn "Escolha o modo de jogo:\n\t[1] Quero um modo mais confortável\n\t[2] Me desafie!"
        mode <- getLine
        putStrLn "Muito bem..." 
        threadDelay 500000
        putStrLn "Vamos lá!"
        actionInGame B.tabuleiro -- (Acho que aqui deveria ser usado como parametro o tabuleiro já com modo)

    -- Autoexplicativo
    clearScreen :: IO ()
    clearScreen = putStr "\ESC[2J\ESC[H"

    -- Ações dentro do jogo
    -- Inserir/Deletar numero do tabuleiro
    -- Falta implementar um modo de saída do game
    actionInGame :: [[Char]] -> IO ()
    actionInGame board = do    
        B.printBoard board
        putStrLn "[I] Inserir um número;\n[D] Deletar um número;\n[R] Encerrar este jogo"
        act <- getLine
        case map toLower act of
            "i" -> do
                newBoard <- insertFlow board
                clearScreen
                actionInGame newBoard
            "d" -> do
                newBoard <- deleteFlow board
                clearScreen
                actionInGame newBoard
            "r" -> do
                putStrLn "Tem certeza que deseja começar de novo? y/n"
                r <- getLine
                case toLower (r !! 0) of
                    'y' -> menu
                    _ -> actionInGame board

            _ -> actionInGame board


    -- Funcao do fluxo para inserir um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    insertFlow :: B.Board -> IO B.Board
    insertFlow board = do
        putStrLn "Digite o número: "
        num <- getLine

        if not (isValidNumber num) then do
            putStrLn "Número inválido (1 a 9)"
            threadDelay 700000
            return board
        else do
            putStrLn "Digite a coordenada"
            coor <- getLine

            if not (isValidCoord coor) then do
                putStrLn "Coordenada inválida (A1 a I9)"
                threadDelay 700000
                return board
            else do
                case (num, coor) of
                    ([n], [l, d]) ->
                        case mapLetterToNumber l of
                            Just row -> do
                                let baseCol = digitToInt d - 1
                                    col
                                        | baseCol >= 6 = baseCol + 2
                                        | baseCol >= 3 = baseCol + 1
                                        | otherwise    = baseCol

                                case insert n row col board of
                                    Left erro -> do
                                        print erro
                                        return board
                                    Right newBoard ->
                                        return newBoard

                            Nothing -> do
                                putStrLn "Coordenada inválida"
                                return board

                    _ -> do
                        putStrLn "Entrada inválida"
                        return board
            

    -- Funcao auxiliar de insercao
    insert :: Char -> Int -> Int -> B.Board -> Either B.BoardError B.Board
    insert num row col board = B.insertCharOnBoard num row col board --AJUSTAR

    -- Funcao do fluxo para remover um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    deleteFlow :: B.Board -> IO B.Board
    deleteFlow board = do
        putStrLn "Digite a coordenada"
        coor <- getLine

        if not (isValidCoord coor) then do
            putStrLn "Coordenada inválida (A1 a I9)"
            threadDelay 700000
            return board
        else do
            case coor of
                    ([l, d]) ->
                        case mapLetterToNumber l of
                            Just row ->
                                let col = digitToInt d
                                in case delete (row - 1) (col - 1) board of
                                    Left erro -> do
                                        print erro
                                        return board
                                    Right newBoard ->
                                        return newBoard
                            Nothing -> do
                                putStrLn "Coordenada inválida"
                                return board
    
        
    -- Função auxiliar de delete
    delete :: Int -> Int -> B.Board -> Either B.BoardError B.Board
    delete row col board = B.deleteCharFromBoard row col board

    -- transforma um Char em Int
    -- faz a verificação se o elemento de entrada for de digito único
    charToInt :: Char -> Maybe Int
    charToInt c
        | isDigit c = Just (digitToInt c)
        | otherwise = Nothing

    -- transgforma uma String ([Char]) de digito único em um tipo Char
    stringToChar :: String -> Maybe Char
    stringToChar [c] = Just c
    stringToChar _   = Nothing

    -- Valida o número de entrada para estar dentro do escopo do tabuleiro 1-9
    isValidNumber :: String -> Bool
    isValidNumber s =
        case readMaybe s :: Maybe Int of
            Just n  -> n >= 1 && n <= 9
            Nothing -> False
    
    -- Valida a coordenada de entrada para estar dentro do escopo do tabuleiro
    -- A0 - I9
    isValidCoord :: String -> Bool
    isValidCoord [l, d] =
        let l' = toLower l
        in  l' >= 'a' && l' <= 'i'
            && d  >= '1' && d  <= '9'
    isValidCoord _ = False

    -- Mapeia as letas A-I aos numeros 0-8
    mapLetterToNumber :: Char -> Maybe Int
    mapLetterToNumber c =
         case toLower c of
        'a' -> Just 0
        'b' -> Just 1
        'c' -> Just 2
        'd' -> Just 4
        'e' -> Just 5
        'f' -> Just 6
        'g' -> Just 8
        'h' -> Just 9
        'i' -> Just 10
        _   -> Nothing

