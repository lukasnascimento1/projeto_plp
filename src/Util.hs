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

    -- Mapeia as letas A-I aos numeros 0-8, considerando as divisorias do tabuleiro (por isso não tem o 3 nem o 7)
    mapLetterToNumber :: Char -> Maybe Int
    mapLetterToNumber c =
        let l = toLower c
        in if l >= 'a' && l <= 'i' then Just (fromEnum l - fromEnum 'a') else Nothing

    -- mapeia os digitos 1-9 para as colunas corretas do tabuleiro, da matriz. Considerando as divisorias do tabuleiro (por isso não tem o 3 nem o 7)
    mapDigitToColumn ::  Char -> Maybe Int
    mapDigitToColumn d =
        let digits = ['1'..'9']
        in if d `elem` digits then Just (fromEnum d - fromEnum '1') else Nothing
