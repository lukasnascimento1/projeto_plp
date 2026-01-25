{-
    Módulo responsável por utilitários gerais do programa, como leitura de entrada
    e validação de dados.
-}
module Util where

    import System.Console.Haskeline
    import System.IO (hFlush, stdout)
    import Data.Char (toLower)

    data InputError
        = InvalidNumber
            | InvalidCoord
            | ImmutableCoord
            | MalformedInput

    -- Autoexplicativo
    clearScreen :: IO ()
    clearScreen = do
        putStr "\ESC[2J\ESC[3J\ESC[H"
        hFlush stdout

    -- Função auxiliar para ler entrada com suporte a backspace
    readUserInput :: String -> IO String
    readUserInput prompt = runInputT defaultSettings $ do
        maybeLine <- getInputLine prompt
        case maybeLine of
            Nothing -> return ""  -- EOF
            Just line -> return line

    -- Valida o número de entrada para estar dentro do escopo do tabuleiro 1-9
    isValidNumber :: String -> Bool
    isValidNumber s = s `elem` map (:[]) ['1'..'9']

    -- Valida a coordenada de entrada para estar dentro do escopo do tabuleiro
    -- A0 - I9
    isValidCoord :: String -> Bool
    isValidCoord [l, d] =
        let l' = toLower l
        in  l' >= 'a' && l' <= 'i'
            && d  >= '1' && d  <= '9'
    isValidCoord _ = False

    -- Valida se o número e coordenada que o usuário quer incluir/alterar são válidos
    validateInsert :: String -> String -> [(Int, Int)] -> Either InputError(Char, Int, Int)
    validateInsert num coor fixed = do
        valor <- if isValidNumber num
            then Right (head num)
            else Left InvalidNumber

        (row, col) <- validateCoord coor fixed
        return (valor, row, col)

    -- Valida se a coordenada pode ser deletada
    validateCoord :: String -> [(Int, Int)] -> Either InputError (Int, Int)
    validateCoord coor fixed = do
        (l, d) <- case coor of
            [l', d'] -> Right (l', d')
            _ -> Left MalformedInput

        row <- maybe (Left InvalidCoord) Right (Util.mapLetterToNumber l)
        col <- maybe (Left InvalidCoord) Right (Util.mapDigitToColumn d)
        if (row, col) `notElem` fixed
            then Right (row, col)
            else Left ImmutableCoord

    -- Mapeia as letas A-I aos numeros 0-8.
    mapLetterToNumber :: Char -> Maybe Int
    mapLetterToNumber c =
        let l = toLower c
        in if l >= 'a' && l <= 'i' then Just (fromEnum l - fromEnum 'a') else Nothing

    -- mapeia os digitos 1-9 para as colunas corretas do tabuleiro, da matriz.
    mapDigitToColumn ::  Char -> Maybe Int
    mapDigitToColumn d =
        let digits = ['1'..'9']
        in if d `elem` digits then Just (fromEnum d - fromEnum '1') else Nothing
