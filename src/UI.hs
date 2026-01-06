

{-
    Interface de interação do jogo
-}
module UI where

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
        


    {-
    Seleciona a primeira ação a partir do Menu incial do jogo:
    A) About sudoku T) Tutorial do game X) Qualquer outra tecla inicia o jogo
    -}
    firstAction :: String -> IO ()
    firstAction action
        | action == "A" || action == "a" = do
            putStrLn about -- descrição do sudoku
            menu
        | action == "T" || action == "t" = do
            putStrLn tutorial -- tutorial das funcoes 
            menu
        | otherwise = startGame


    -- Não lembro agora mas acho que isso aqui pode apagar kk
    selectCell :: IO Cell
    selectCell = do
        putStrLn "-> "
        cell <- getLine
        if cell == "H" 
            then do
                putStrLn tutorial
                selectCell
            else 
                return cell
    
    -- Inicia o modo de jogo
    startGame :: IO ()
    startGame = do
        mode <- chooseMode
        -- putStrLn sudoku mode --tabuleiro gerado a partir do modo escolhido (sudoku seria a funcao)
        putStrLn "Muito bem..." 
        threadDelay 500000
        putStrLn "Vamos lá!"
        actionInGame B.tabuleiro

    -- Autoexplicativo
    clearScreen :: IO ()
    clearScreen = putStr "\ESC[2J\ESC[H"


    -- Ações dentro do jogo
    -- Inserir/Deletar numero do tabuleiro
    -- Falta implementar um modo de saída do game
    actionInGame :: [[Char]] -> IO ()
    actionInGame board = do    
        B.printBoard board
        putStrLn "[I] Inserir um número;\n[D] Deletar um número;"
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
            _ -> actionInGame board


    -- Funcao do fluxo para inserir um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    insertFlow :: B.Board -> IO B.Board
    insertFlow board = do
        putStrLn "Digite o numero: "
        num <- getLine
        putStrLn "Digite a coordenada"
        coor <- getLine

        let row = (digitToInt(coor !! 0)) 
            col = (digitToInt(coor !! 1))
        
        case num of
            [n] ->
                case insert n row col board of
                    Left erro -> do
                        print erro -- print é o print para os dev / putStrLn é para o user
                        return board
                    Right newBoard -> return newBoard
            _ -> do
                putStrLn "Número Inválido"
                -- B.printBoard board
                return board
        

    -- Funcao auxiliar de insercao
    insert :: Char -> Int -> Int -> B.Board -> Either B.BoardError B.Board
    insert num row col board = B.insertCharOnBoard num row col board --AJUSTAR

    -- Funcao do fluxo para remover um elemento
    -- retorna IO Board para ser usado no actionInGame
    -- assim é possível visualizar a atualização do tabuleiro
    deleteFlow :: B.Board -> IO B.Board
    deleteFlow board = do
        putStrLn "Digite a coordenada:"
        coor <- getLine
        let row = (digitToInt(coor !! 0)) 
            col = (digitToInt(coor !! 1))
        
        case delete row col board of
            Left erro -> do
                print erro
                return board
            Right newBoard -> return newBoard
        
    -- Função auxiliar de delete
    delete :: Int -> Int -> B.Board -> Either B.BoardError B.Board
    delete row col board = B.deleteCharFromBoard row col board

    -- transforma um Char em Int
    -- faz a verificação se o elemento de entrada for de digito único
    charToInt :: Char -> Maybe Int
    charToInt c
        | isDigit c = Just (digitToInt c)
        | otherwise = Nothing

    -- Função abstrata para imprimir o menu de escolha do modo de jogo
    -- retorna um IO String mas possa ser que precisemos mudar para String somente
    chooseMode :: IO String
    chooseMode = do
        putStrLn "Escolha o modo de jogo:\n\t[1] Quero um modo mais confortável\n\t[2] Me desafie!"
        getLine
    
    -- Funcao abstrata para imprimir o menu de acoes do jogo
    -- retorna um IO String mas possa ser que precisemos mudar para String somente 
    selectFunction :: IO String
    selectFunction = do
        putStrLn menuInicial
        getLine

