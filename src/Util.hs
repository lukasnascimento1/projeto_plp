module Util where

    import System.Console.Haskeline
    import Data.Char (toLower)

    -- Autoexplicativo
    clearScreen :: IO ()
    clearScreen = putStr "\ESC[2J\ESC[H"

    -- Função auxiliar para ler entrada com suporte a backspace
    readUserInput :: String -> IO String
    readUserInput prompt = runInputT defaultSettings $ do
        maybeLine <- getInputLine prompt
        case maybeLine of
            Nothing -> return ""  -- EOF
            Just line -> return line

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