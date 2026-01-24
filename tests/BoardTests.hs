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
  let expectedRow = "........."
  assertEqual "Linha 0 deve estar vazia" expectedRow (getRowFromBoard 0 board)
  assertEqual "Linha 1 deve estar vazia" expectedRow (getRowFromBoard 1 board)
  assertEqual "Linha 2 deve estar vazia" expectedRow (getRowFromBoard 2 board)        
  assertEqual "Linha 3 deve estar vazia" expectedRow (getRowFromBoard 3 board)
  assertEqual "Linha 4 deve estar vazia" expectedRow (getRowFromBoard 4 board)
  assertEqual "Linha 5 deve estar vazia" expectedRow (getRowFromBoard 5 board)  
  assertEqual "Linha 6 deve estar vazia" expectedRow (getRowFromBoard 6 board)      
  assertEqual "Linha 7 deve estar vazia" expectedRow (getRowFromBoard 7 board)
  assertEqual "Linha 8 deve estar vazia" expectedRow (getRowFromBoard 8 board)


test_insertCharOnRowValid :: Test
test_insertCharOnRowValid = TestCase $ do
  let row = "........."

  assertEqual "Pos 0" "5........" (insertCharOnRow '5' 0 row)
  assertEqual "Pos 1" ".5......." (insertCharOnRow '5' 1 row)
  assertEqual "Pos 2" "..5......" (insertCharOnRow '5' 2 row)
  assertEqual "Pos 4" "....5...." (insertCharOnRow '5' 4 row)
  assertEqual "Pos 5" ".....5..." (insertCharOnRow '5' 5 row)
  assertEqual "Pos 6" "......5.." (insertCharOnRow '5' 6 row)
  assertEqual "Pos 8" "........5" (insertCharOnRow '5' 8 row)
 


test_insertCharOnRowInvalid :: Test
test_insertCharOnRowInvalid = TestCase $ do
  let row = "........."

  assertEqual "Índice negativo"
    row
    (insertCharOnRow '9' (-1) row)

  assertEqual "Índice maior que tamanho"
    row
    (insertCharOnRow '5' 20 row)


test_insertCharOnBoardAllValid :: Test
test_insertCharOnBoardAllValid = TestCase $ do
  let board = emptyBoard
  case insertCharOnBoard '7' 0 0 board of
    Right b  -> assertEqual "Inseriu corretamente" '7' ((b !! 0) !! 0)
    _        -> assertFailure "Esperava Right"

test_insertCharOnBoardInvalid :: Test
test_insertCharOnBoardInvalid = TestCase $ do
  let board = emptyBoard

  assertEqual "Linha inválida" (Left InvalidCoordinates) (insertCharOnBoard '7' 9 0 board)
  assertEqual "Coluna inválida" (Left InvalidCoordinates) (insertCharOnBoard '7' 0 9 board)
  assertEqual "Negativo" (Left InvalidCoordinates) (insertCharOnBoard '7' (-1) 0 board)

test_deleteCharFromBoardAllValid :: Test
test_deleteCharFromBoardAllValid = TestCase $ do
  let (Right board1) = insertCharOnBoard '5' 2 2 emptyBoard
  let (Right board2) = deleteCharFromBoard 2 2 board1
  assertEqual "Removeu corretamente" '.' ((board2 !! 2) !! 2)


test_deleteCharFromBoardInvalid :: Test
test_deleteCharFromBoardInvalid = TestCase $ do
  let board = emptyBoard
  assertEqual "Linha inválida" (Left InvalidCoordinates) (deleteCharFromBoard 9 0 board)
  assertEqual "Coluna inválida" (Left InvalidCoordinates) (deleteCharFromBoard 0 9 board)
  assertEqual "Negativo" (Left InvalidCoordinates) (deleteCharFromBoard (-1) 0 board)


test_validateCoordenates :: Test
test_validateCoordenates = TestCase $ do
  assertBool "Coordenada válida (0,0)" (validateCoordenates 0 0)
  assertBool "Coordenada válida (8,8)" (validateCoordenates 8 8)
  assertBool "Linha inválida" (not (validateCoordenates 9 0))
  assertBool "Coluna inválida" (not (validateCoordenates 0 9))
  assertBool "Fora do tabuleiro" (not (validateCoordenates 11 0))