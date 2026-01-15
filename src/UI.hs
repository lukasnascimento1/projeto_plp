

{-
    Interface de interação do jogo
-}
module UI where

    import Text.Read (readMaybe)
    import Data.Char (digitToInt, isDigit, toLower)
    import Control.Concurrent (threadDelay)
    import System.Exit (exitSuccess)
    import System.IO (hFlush, stdout)
    import qualified Board as B
    import qualified Generator as G
    import qualified Util

    type Cell = String --[Char]


    tutorial = "[I] Inserir um número;"
            ++"\n[D] Deletar um número;"
            ++"\nAo selecionar essas ações você precisará inserir as coordenadas da jogada ('A1', 'D7', 'I9')"
            ++"\n[H] Help"
            ++"\n[R] Restart"

    menuInicial = "[A] Sobre o Sudoku\n[T] Tutorial\nQualquer outra tecla inicia o jogo\n"
    finalizou = "Yeah!!! Você finalizou este SUDOKU!"
    about = "Sudoku é um jogo de lógica em que se preenche uma grade 9×9 com números de 1 a 9, sem repetir valores em linhas, colunas e regiões 3×3."


    menu :: IO String
    menu = do
        putStrLn "Pratique seu raciocínio lógico com: \n------>\tSUDOKU!\t<------"
        threadDelay 500000
        putStrLn ""
        putStrLn menuInicial
        action <- Util.readUserInput "> "
        firstAction action

    menuFinal :: String -> IO String
    menuFinal option = do
        case option of
            "quit" -> return ""
            _ -> do
                putStrLn "Parabéns!!!\nVocê Finalizou um SUDOKU\nDeseja jogar novamente? y/n"
                r <- Util.readUserInput ""
                case map toLower r of
                    "y" -> menu
                    _ -> return ""


    {-
    Seleciona a primeira ação a partir do Menu incial do jogo:
    A) About sudoku T) Tutorial do game X) Qualquer outra tecla inicia o jogo
    -}
    firstAction :: String -> IO String
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
    startGame :: IO String
    startGame = do
        putStrLn "Escolha o modo de jogo:\n\t[1] Quero um modo mais confortável\n\t[2] Me desafie!"
        mode <- Util.readUserInput "> "
        --putStrLn "teste"
        tabuleiro <- case mode of
            "1" -> G.generateEasy
            "2" -> G.generateHard
            _ -> G.generateEasy
        putStrLn "Muito bem..."
        threadDelay 500000
        putStrLn "Vamos lá!"
        --actionInGame B.tabuleiro -- (Acho que aqui deveria ser usado como parametro o tabuleiro já com modo)
        let fixedNumbers = getFilledPositions tabuleiro
        actionInGame tabuleiro fixedNumbers

    -- Ações dentro do jogo
    -- Inserir/Deletar numero do tabuleiro
    -- Menu com opções de continuar jogando, recomeçar ou sair
    actionInGame :: [[Char]] -> [(Int, Int)] -> IO String
    actionInGame board fixedNumbers = do
        B.printBoard board
        putStrLn "[I] Inserir um número;\n[D] Deletar um número;\n[R] Encerrar este jogo\n[Q] Sair do programa"
        act <- Util.readUserInput "> "
        case map toLower act of
            "i" -> do
                newBoard <- insertFlow board fixedNumbers
                Util.clearScreen
                actionInGame newBoard fixedNumbers
            "d" -> do
                newBoard <- deleteFlow board fixedNumbers
                Util.clearScreen
                actionInGame newBoard fixedNumbers
            "r" -> do
                putStrLn "Tem certeza que deseja começar de novo? y/n"
                r <- Util.readUserInput ""
                case toLower (r !! 0) of
                    'y' -> menu
                    _ -> actionInGame board fixedNumbers
            -- Nova opção: Q para sair do programa
            "q" -> do
                putStrLn "Tem certeza que deseja sair do programa? y/n"
                r <- Util.readUserInput ""
                case toLower (r !! 0) of
                    'y' -> do
                        putStrLn "Até logo! Obrigado por jogar SUDOKU!"
                        threadDelay 500000  -- Aguarda 0.5s antes de sair
                        -- exitSuccess  -- Encerra o programa completamente, ma não consegui fazer que não aparecesse a mensagem de exeption
                        Util.clearScreen
                        return ""
                    _ -> actionInGame board fixedNumbers

            _ -> actionInGame board fixedNumbers


    -- Funcao do fluxo para inserir um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    insertFlow :: B.Board -> [(Int, Int)] -> IO B.Board
    insertFlow board fixedNumbers = do
        putStrLn "Digite o número: "
        num <- Util.readUserInput ""

        if not (isValidNumber num) then do
            putStrLn "Número inválido (1 a 9)"
            threadDelay 700000
            return board
        else do
            putStrLn "Digite a coordenada"
            coor <- Util.readUserInput ""

            if not (Util.isValidCoord coor) then do
                putStrLn "Coordenada inválida (A1 a I9)"
                threadDelay 700000
                return board
            else do
                if not (verifyCoord coor fixedNumbers) then do
                    putStrLn "Essas coordenadas são imutáveis!"
                    threadDelay 700000
                    return board
                else do
                    case (num, coor) of
                        ([n], [l, d]) ->
                            case Util.mapLetterToNumber l of
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
    deleteFlow :: B.Board -> [(Int, Int)] -> IO B.Board
    deleteFlow board fixedNumbers = do
        putStrLn "Digite a coordenada"
        coor <- Util.readUserInput ""

        if not (Util.isValidCoord coor) then do
            putStrLn "Coordenada inválida (A1 a I9)"
            threadDelay 700000
            return board
        else do
            if verifyCoord coor fixedNumbers then do
                case coor of
                    [l, d] ->
                        case Util.mapLetterToNumber l of
                            Just row -> do
                                let baseCol = digitToInt d - 1
                                    col
                                        | baseCol >= 6 = baseCol + 2
                                        | baseCol >= 3 = baseCol + 1
                                        | otherwise    = baseCol

                                case delete row col board of
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
            else do
                    putStrLn "Essas coordenadas são imutáveis!"
                    threadDelay 700000
                    return board


    -- Função auxiliar de delete
    delete :: Int -> Int -> B.Board -> Either B.BoardError B.Board
    delete row col board = B.deleteCharFromBoard row col board

    -- Valida o número de entrada para estar dentro do escopo do tabuleiro 1-9
    isValidNumber :: String -> Bool
    isValidNumber s =
        case readMaybe s :: Maybe Int of
            Just n  -> n >= 1 && n <= 9
            Nothing -> False

    getFilledPositions :: [[Char]] -> [(Int, Int)]
    getFilledPositions board = [(r, c) | r <- valid, c <- valid, B.validateCoordenates r c, (board !! r !! c) /= 'x']
        where valid = [0, 1, 2, 4, 5, 6, 8, 9, 10]

    -- retorna True se a coordenada não esrtiver na lista
    verifyCoord :: String -> [(Int, Int)] -> Bool
    verifyCoord [row, col] listCoord =
        case (Util.mapLetterToNumber row, Util.mapDigitToColumn col) of
            (Just r, Just c) -> (r, c) `notElem` listCoord
            _ -> False
    verifyCoord _ _ = False