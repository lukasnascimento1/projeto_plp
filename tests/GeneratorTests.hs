module GeneratorTests where
import Test.HUnit
import Board
import Generator

-- Suite de testes do Generator
tests_Generator :: Test
tests_Generator = TestList
  [ TestLabel "getColumn" test_getColumn
  , TestLabel "getBox" test_getBox
  , TestLabel "isPositionValid" test_isPositionValid
  , TestLabel "findEmptyPositions" test_findEmptyPositions
  , TestLabel "deleteCharFromBoard invalid" test_deleteCharFromBoardInvalidInGenerator
  ]

-- Testa getColumn ignorando as linhas separadoras
test_getColumn :: Test
test_getColumn = TestCase $ do
  let board = emptyBoard
  let col0 = getColumn 0 board
  let expected = ['x','x','x','x','x','x','x','x','x']  -- linhas 3 e 7 ignoradas
  assertEqual "Coluna 0 deve ignorar divisórias" expected col0

-- Testa getBox
test_getBox :: Test
test_getBox = TestCase $ do
  let board = emptyBoard
  let box0 = getBox 0 0 board
  let expected = replicate 9 'x'
  assertEqual "Box 0,0 deve ser só 'x'" expected box0

  let boxMiddle = getBox 5 5 board
  assertEqual "Box 5,5 deve ser só 'x'" (replicate 9 'x') boxMiddle

-- Testa isPositionValid
test_isPositionValid :: Test
test_isPositionValid = TestCase $ do
  let board = emptyBoard
  -- posição vazia deve ser válida
  assertBool "Posição (0,0) válida" (isPositionValid '1' 0 0 board)
  -- inserir número na posição e tentar a mesma linha
  let (Right board1) = insertCharOnBoard '1' 0 0 board
  assertBool "Mesma linha não é válida" (not $ isPositionValid '1' 0 1 board1)
  -- mesma coluna
  assertBool "Mesma coluna não é válida" (not $ isPositionValid '1' 1 0 board1)
  -- mesma box
  assertBool "Mesma box não é válida" (not $ isPositionValid '1' 1 1 board1)

-- Testa findEmptyPositions
test_findEmptyPositions :: Test
test_findEmptyPositions = TestCase $ do
  let board = emptyBoard
  let empties = findEmptyPositions board
  -- deve conter todas as posições válidas
  let validIdx = [0,1,2,4,5,6,8,9,10]
  let expected = [(r,c) | r <- validIdx, c <- validIdx]
  assertEqual "Todas as posições válidas estão vazias" expected empties

-- Testa deleteCharFromBoard via Generator
test_deleteCharFromBoardInvalidInGenerator :: Test
test_deleteCharFromBoardInvalidInGenerator = TestCase $ do
  let board = emptyBoard
  -- linha negativa
  assertEqual "Linha negativa retorna erro" (Left InvalidCoordinates) (deleteCharFromBoard (-1) 0 board)
  -- coluna negativa
  assertEqual "Coluna negativa retorna erro" (Left InvalidCoordinates) (deleteCharFromBoard 0 (-1) board)
  -- linha separadora
  assertEqual "Linha separadora retorna erro" (Left InvalidCoordinates) (deleteCharFromBoard 3 0 board)
  -- coluna separadora
  assertEqual "Coluna separadora retorna erro" (Left InvalidCoordinates) (deleteCharFromBoard 0 7 board)
