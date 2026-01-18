module BoardTests where
import Test.HUnit
import Board


-- inicio dos testes
tests_Board :: Test
tests_Board = TestList
  [ TestLabel "Testa se o tabuleiro inicial está vazio" test_initialBoardEmpty
  , TestLabel "insertCharOnRow - posições válidas" test_insertCharOnRowValid
  , TestLabel "insertCharOnRow - posições inválidas" test_insertCharOnRowInvalid
  , TestLabel "insertCharOnBoard - todas posições válidas" test_insertCharOnBoardAllValid
  , TestLabel "insertCharOnBoard - posições inválidas" test_insertCharOnBoardInvalid
  , TestLabel "deleteCharFromBoard - todas posições válidas" test_deleteCharFromBoardAllValid
  , TestLabel "deleteCharFromBoard - posições inválidas" test_deleteCharFromBoardInvalid
  , TestLabel "validateCoordenates" test_validateCoordenates
  ]

test_initialBoardEmpty :: Test
test_initialBoardEmpty = TestCase $ do
  let board = emptyBoard
  let expectedRow = "xxx|xxx|xxx"
  assertEqual "Linha 0 deve estar vazia" expectedRow (getRowFromBoard 0 board)
  assertEqual "Linha 1 deve estar vazia" expectedRow (getRowFromBoard 1 board)
  assertEqual "Linha 2 deve estar vazia" expectedRow (getRowFromBoard 2 board)        
  assertEqual "Linha 4 deve estar vazia" expectedRow (getRowFromBoard 4 board)
  assertEqual "Linha 5 deve estar vazia" expectedRow (getRowFromBoard 5 board)
  assertEqual "Linha 6 deve estar vazia" expectedRow (getRowFromBoard 6 board)  
  assertEqual "Linha 8 deve estar vazia" expectedRow (getRowFromBoard 8 board)      
  assertEqual "Linha 9 deve estar vazia" expectedRow (getRowFromBoard 9 board)
  assertEqual "Linha 10 deve estar vazia" expectedRow (getRowFromBoard 10 board)


test_insertCharOnRowValid :: Test
test_insertCharOnRowValid = TestCase $ do
  let row = "xxx|xxx|xxx"

  assertEqual "Pos 0" "5xx|xxx|xxx" (insertCharOnRow '5' 0 row)
  assertEqual "Pos 1" "x5x|xxx|xxx" (insertCharOnRow '5' 1 row)
  assertEqual "Pos 2" "xx5|xxx|xxx" (insertCharOnRow '5' 2 row)
  assertEqual "Pos 4" "xxx|5xx|xxx" (insertCharOnRow '5' 4 row)
  assertEqual "Pos 5" "xxx|x5x|xxx" (insertCharOnRow '5' 5 row)
  assertEqual "Pos 6" "xxx|xx5|xxx" (insertCharOnRow '5' 6 row)
  assertEqual "Pos 8" "xxx|xxx|5xx" (insertCharOnRow '5' 8 row)
  assertEqual "Pos 9" "xxx|xxx|x5x" (insertCharOnRow '5' 9 row)
  assertEqual "Pos 10" "xxx|xxx|xx5" (insertCharOnRow '5' 10 row)


test_insertCharOnRowInvalid :: Test
test_insertCharOnRowInvalid = TestCase $ do
  let row = "xxx|xxx|xxx"

  assertEqual "Índice negativo"
    row
    (insertCharOnRow '5' (-1) row)

  assertEqual "Índice maior que tamanho"
    row
    (insertCharOnRow '5' 20 row)


test_insertCharOnBoardAllValid :: Test
test_insertCharOnBoardAllValid = TestCase $ do
  let board = emptyBoard
  let validIdx = [0,1,2,4,5,6,8,9,10]

  let results =
        [ insertCharOnBoard '9' r c board
        | r <- validIdx
        , c <- validIdx
        ]

  assertBool "Todas inserções válidas retornam Right"
    (all isRight results)

  where
    isRight (Right _) = True
    isRight _         = False


test_insertCharOnBoardInvalid :: Test
test_insertCharOnBoardInvalid = TestCase $ do
  let board = emptyBoard

  assertEqual "Linha separadora"
    (Left InvalidCoordinates)
    (insertCharOnBoard '7' 3 0 board)

  assertEqual "Coluna separadora"
    (Left InvalidCoordinates)
    (insertCharOnBoard '7' 0 7 board)

  assertEqual "Ambos separadores"
    (Left InvalidCoordinates)
    (insertCharOnBoard '7' 3 7 board)

  assertEqual "Fora do tabuleiro"
    (Left InvalidCoordinates)
    (insertCharOnBoard '7' 11 0 board)

  assertEqual "Coluna negativa"
    (Left InvalidCoordinates)
    (insertCharOnBoard '7' 0 (-1) board)

test_deleteCharFromBoardAllValid :: Test
test_deleteCharFromBoardAllValid = TestCase $ do
  let board = emptyBoard
  let validIdx = [0,1,2,4,5,6,8,9,10]

  let boardWithNums =
        foldl
          (\b (r,c) -> case insertCharOnBoard '8' r c b of
                          Right nb -> nb
                          _ -> b)
          board
          [(r,c) | r <- validIdx, c <- validIdx]

  let results =
        [ deleteCharFromBoard r c boardWithNums
        | r <- validIdx
        , c <- validIdx
        ]

  assertBool "Todas remoções válidas retornam Right"
    (all isRight results)

  where
    isRight (Right _) = True
    isRight _         = False


test_deleteCharFromBoardInvalid :: Test
test_deleteCharFromBoardInvalid = TestCase $ do
  let board = emptyBoard

  assertEqual "Linha separadora"
    (Left InvalidCoordinates)
    (deleteCharFromBoard 3 0 board)

  assertEqual "Coluna separadora"
    (Left InvalidCoordinates)
    (deleteCharFromBoard 0 7 board)

  assertEqual "Fora do tabuleiro"
    (Left InvalidCoordinates)
    (deleteCharFromBoard 12 1 board)


test_validateCoordenates :: Test
test_validateCoordenates = TestCase $ do
  assertBool "Coordenada válida (0,0)" (validateCoordenates 0 0)
  assertBool "Coordenada válida (10,10)" (validateCoordenates 10 10)
  assertBool "Linha separadora inválida" (not (validateCoordenates 3 0))
  assertBool "Coluna separadora inválida" (not (validateCoordenates 0 7))
  assertBool "Fora do tabuleiro" (not (validateCoordenates 11 0))