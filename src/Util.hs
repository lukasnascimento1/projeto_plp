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

    -- mapeia os digitos 1-9 para as colunas corretas do tabuleiro, da matriz.
    mapDigitToColumn ::  Char -> Maybe Int
    mapDigitToColumn d =
        case d of
        '1' -> Just 0
        '2' -> Just 1
        '3' -> Just 2
        '4' -> Just 4
        '5' -> Just 5
        '6' -> Just 6
        '7' -> Just 8
        '8' -> Just 9
        '9' -> Just 10
        _   -> Nothing