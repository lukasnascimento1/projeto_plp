{-
    Interface de interação do jogo
-}

module UI where

    import Text.Read (readMaybe)
    import Data.Char (digitToInt, isDigit, toLower)
    import Control.Concurrent (threadDelay)
    import System.Exit (exitSuccess)
    import System.IO (hFlush, stdout)
    import Board (Board)
    import qualified Board as B
    import qualified Generator as G
    import qualified Validation as V
    import qualified Util

    textColorWhiteWeak = "\ESC[2;37m"
    textColorGreenWeak = "\ESC[2;32m"
    textColorBlueWeak = "\ESC[2;34m"
    textColorCyanWeak = "\ESC[2;36m"
    textColorGreen = "\ESC[92m"
    textColorYellow = "\ESC[93m"
    textBold = "\ESC[1m"
    resetColor = "\ESC[0m"

    type Cell = String --[Char]

    data GameState = GameState
        {
            initialBoard :: Board, --imutavel
            currentBoard :: Board
        }

    tutorial = textColorYellow ++ "[I] " ++ resetColor ++ "Inserir um número"
            ++"\n" ++ textColorYellow ++ "[D] " ++ resetColor ++ "Deletar um número"
            ++"\nAo selecionar essas ações você precisará inserir as coordenadas da jogada ('A1', 'D7', 'I9')"
            ++"\n" ++ textColorYellow ++ "[V] " ++ resetColor ++ "Verificar solução"
            ++"\n" ++ textColorYellow ++ "[D] " ++ resetColor ++ " Restart"

    menuInicial = textColorYellow ++ "[A] " ++ resetColor ++ " Sobre o Sudoku\n"
                ++ textColorYellow ++ "[T] " ++ resetColor ++" Tutorial\nQualquer outra tecla inicia o jogo\n"
    
    about = "SUDOKU é um jogo de lógica em que se preenche uma grade 9×9 com números de 1 a 9, sem repetir valores em linhas, colunas e regiões 3×3."

    -- movi a lógica de impressão para o módulo de interface
    printBoard :: Board -> Board -> IO ()
    printBoard initial current = do
        putStrLn cabecalho
        putStrLn divisoria
        mapM_ printRow (zip3 [0..8] initial current)

      where
        cabecalho = "\ESC[1;35m    1 2 3   4 5 6   7 8 9\ESC[0m"
        divisoria = "  +-------+-------+-------+"
        
        printRow (idx, rowIni, rowCur) = do
            let letra = ['A'..'I'] !! idx
            let cells = zipWith colorCell rowIni rowCur
            let p1 = unwords (take 3 cells)
            let p2 = unwords (take 3 (drop 3 cells))
            let p3 = unwords (take 3 (drop 6 cells))
            putStrLn $ [letra] ++ " | " ++ p1 ++ " | " ++ p2 ++ " | " ++ p3 ++ " |"
            if idx `elem` [2, 5, 8]
                then putStrLn divisoria
                else return ()

        colorCell ini cur
            | cur == 'x' = textColorWhiteWeak ++ "x" ++ resetColor
            | ini /= 'x' = textColorCyanWeak ++ [cur] ++ resetColor 
            | otherwise  = textBold ++ textColorGreen ++ [cur] ++ resetColor

    menu :: IO String
    menu = do
        Util.clearScreen
        putStrLn "Pratique seu raciocínio lógico com:\n------>\tSUDOKU!\t<------"
        threadDelay 500000
        putStrLn ""
        putStrLn menuInicial
        action <- Util.readUserInput "> "
        firstAction action

    {-
    Seleciona a primeira ação a partir do Menu incial do jogo:
    A) About sudoku T) Tutorial do game X) Qualquer outra tecla inicia o jogo
    -}
    firstAction :: String -> IO String
    firstAction action = case map toLower action of
        "a" -> do
            putStrLn about
            putStrLn "\nPressione Enter para voltar..."
            _ <- getLine
            menu
        "t" -> do
            putStrLn tutorial
            putStrLn "\nPressione Enter para voltar..."
            _ <- getLine
            menu
        _ -> startGame

    -- Atualiza o GameState
    initGame :: Board -> IO GameState
    initGame board =
        return GameState 
            {   initialBoard = board,
                currentBoard = board
            }
    
    -- atualiza o valor do tabuleiro atual em GameState
    updateGameState :: GameState -> Board -> GameState
    updateGameState gameState newBoard =
        gameState {currentBoard = newBoard}

    -- Inicia o modo de jogo
    startGame :: IO String
    startGame = do
        putStrLn $ 
                "Escolha o modo de jogo:\n\t" ++
                textColorYellow ++ "[1] " ++ resetColor ++ " Quero um modo mais confortável\n\t" ++
                textColorYellow ++ "[2] " ++ resetColor ++ " Me desafie!"
        mode <- Util.readUserInput "> "
        
        tabuleiro <- case mode of
            "1" -> G.generateEasy
            "2" -> G.generateHard
            _ -> G.generateEasy
        
        gameState <- initGame tabuleiro
        putStrLn "Muito bem... Vamos lá!"
        threadDelay 1000000
        Util.clearScreen
        let fixedNumbers = getFilledPositions tabuleiro
        actionInGame gameState tabuleiro fixedNumbers

    -- Ações dentro do jogo
    -- Inserir/Deletar numero do tabuleiro
    -- Menu com opções de continuar jogando, recomeçar ou sair
    actionInGame :: GameState -> [[Char]] -> [(Int, Int)] -> IO String
    actionInGame gameState board fixedNumbers = do
        printBoard (initialBoard gameState) (currentBoard gameState)
        putStrLn $
                textColorYellow ++ "[I] " ++ resetColor ++ " Inserir um número\n" ++
                textColorYellow ++ "[D] " ++ resetColor ++ " Deletar um número\n" ++
                textColorYellow ++ "[R] " ++ resetColor ++ " Encerrar este jogo\n" ++
                textColorYellow ++ "[V] " ++ resetColor ++ " Verificar solução\n" ++
                textColorYellow ++ "[Q] " ++ resetColor ++ " Sair do programa"
        act <- Util.readUserInput "> "
        case map toLower act of
            "i" -> do
                newBoard <- insertFlow board fixedNumbers
                Util.clearScreen
                let gs = updateGameState gameState newBoard
                actionInGame gs newBoard fixedNumbers
            "d" -> do
                newBoard <- deleteFlow board fixedNumbers
                Util.clearScreen
                let gs = updateGameState gameState newBoard
                actionInGame gs newBoard fixedNumbers
            "r" -> do
                putStrLn "Tem certeza que deseja começar de novo? y/n"
                r <- Util.readUserInput ""
                Util.clearScreen
                case map toLower r of
                        ('y':_) -> do
                            menu
                        _ -> do
                            let gs = updateGameState gameState board
                            actionInGame gs board fixedNumbers
            "v" -> do
                putStrLn "Verificando solução..."
                if V.isSolutionValid board
                    then do
                    putStrLn "Parabéns! Você finalizou esse SUDOKU!"
                    putStrLn "Deseja voltar ao menu? y/n"
                    r <- Util.readUserInput ""
                    Util.clearScreen
                    case map toLower r of
                        ('y':_) -> do
                            menu
                        _ -> do
                            actionInGame gameState board fixedNumbers
                    else do
                    putStrLn "Ops... ainda tem algo errado ou faltando."
                    threadDelay 1000000
                    Util.clearScreen
                    actionInGame gameState board fixedNumbers
            -- Nova opção: Q para sair do programa
            "q" -> do
                putStrLn "Tem certeza que deseja sair do programa? y/n"
                r <- Util.readUserInput ""
                case map toLower r of
                    ('y':_) -> do
                        putStrLn "Até logo! Obrigado por jogar SUDOKU!"
                        threadDelay 1000000  -- Aguarda 1s antes de sair
                        -- exitSuccess  -- Encerra o programa completamente, ma não consegui fazer que não aparecesse a mensagem de exeption
                        Util.clearScreen
                        return ""
                    _ -> do
                        Util.clearScreen
                        actionInGame gameState board fixedNumbers
            _ -> do
                Util.clearScreen
                actionInGame gameState board fixedNumbers

    -- Funcao do fluxo para inserir um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    insertFlow :: B.Board -> [(Int, Int)] -> IO B.Board
    insertFlow board fixedNumbers = do
        putStrLn "Digite o número (1-9): "
        num <- Util.readUserInput ""
        putStrLn "Digite a coordenada (A1-I9):"
        coor <- Util.readUserInput ""

        case Util.validateInsert num coor fixedNumbers of
            Left err -> do
                case err of
                    Util.InvalidNumber  -> putStrLn "Número inválido (1 a 9)"
                    Util.InvalidCoord   -> putStrLn "Coordenada inválida (A1 a I9)"
                    Util.ImmutableCoord -> putStrLn "Essa coordenada é imutável!"
                    Util.MalformedInput -> putStrLn "Entrada inválida"
                threadDelay 1000000
                return board
                
            Right (n, r, c) -> 
                case B.insertCharOnBoard n r c board of
                    Left _ -> do
                        putStrLn "Erro na inserção"
                        return board
                    Right nb -> return nb

    -- Funcao do fluxo para remover um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    deleteFlow :: B.Board -> [(Int, Int)] -> IO B.Board
    deleteFlow board fixedNumbers = do
        putStrLn "Digite a coordenada:"
        coor <- Util.readUserInput ""

        case Util.validateCoord coor fixedNumbers of
            Left err -> do
                case err of
                    Util.InvalidCoord   -> putStrLn "Coordenada inválida (A1 a I9)"
                    Util.ImmutableCoord -> putStrLn "Essa coordenada é imutável!"
                    Util.MalformedInput -> putStrLn "Entrada inválida"
                threadDelay 1000000
                return board
            
            Right (row, col) -> do
                case B.deleteCharFromBoard row col board of
                    Left _ -> do
                        putStrLn "Erro na exclusão"
                        return board
                    Right nb -> return nb

    -- Retorna as posições preenchidas do tabuleiro
    getFilledPositions :: [[Char]] -> [(Int, Int)]
    getFilledPositions board = [(r, c) | r <- [0..8], c <- [0..8], (board !! r !! c) /= 'x']

    -- retorna True se a coordenada não esrtiver na lista
    verifyCoord :: String -> [(Int, Int)] -> Bool
    verifyCoord [row, col] listCoord =
        case (Util.mapLetterToNumber row, Util.mapDigitToColumn col) of
            (Just r, Just c) -> (r, c) `notElem` listCoord
            _ -> False
    verifyCoord _ _ = False
