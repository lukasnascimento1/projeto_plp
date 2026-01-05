

{-
    Interface de interação do jogo
-}
module UI where

    import Control.Concurrent (threadDelay)

    type Cell = String --[Char]

    tutorial = "Selecione a função [I] introduzir as coordenadas da sua jogada\nAs coordenadas são do tipo A1, ou D7"
    functions = "[1] Começar o jogo\n[2] Sobre o Sudoku\n[3] Tutorial"
    finalizou = "Yeah!!! Você finalizou este SUDOKU!"

    startGame :: IO ()
    startGame = do
        putStrLn "Pratique seu raciocínio lógico com: \n------>\tSUDOKU!"
        threadDelay 500000
        putStrLn ""
        f <- selectFunction
        nextAction f
        putStrLn "Muito bem..." 
        threadDelay 500000
        putStrLn "Vamos lá!"


    chooseMode :: IO String
    chooseMode = do
        putStrLn "Escolha o modo de jogo:\n\t[1] Quero um modo mais confortável\n\t[2] Me desafie!"
        getLine


    selectCell :: IO Cell
    selectCell = do
        putStrLn "-> "
        cell <- getLine
        if cell == "S" 
            then do
                putStrLn tutorial
                selectCell
            else 
                return cell
    
    
    selectFunction :: IO String
    selectFunction = do
        putStrLn functions
        getLine


    nextAction :: String -> IO ()
    nextAction action
        | action == "1" = chooseMode >> return ()
        | action == "2" = putStrLn "SUDOKU é um jogo de tabuleiro que voce precisa por uns numeros aí"
        | action == "3" = putStrLn tutorial
        | otherwise     = startGame
