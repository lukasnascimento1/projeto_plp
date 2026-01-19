module ValidationTests where

import Test.HUnit
import Validation
import Board (emptyBoard)

tests_Validation :: Test
tests_Validation  = TestList
    [
        TestCase $ assertBool "Linha válida" (isSequenceValid "569|481|237"),
        TestCase $ assertBool "Linha inválida com x" (not (isSequenceValid "5x9|481|237")),
        TestCase $ assertBool "Linha inválida com duplicata" (not (isSequenceValid "569|461|237")),
        TestCase $ assertBool "Tabuleiro vazio é inválido" (not (isSolutionValid emptyBoard))
    ]