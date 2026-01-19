module Main where

import Test.HUnit
import BoardTests (tests_Board)
import GeneratorTests (tests_Generator)
import ValidationTests (tests_Validation)

main :: IO ()
main = do
    putStrLn "==== Rodando testes Board ===="
    _ <- runTestTT tests_Board
    putStrLn "==== Rodando testes Generator ===="
    _ <- runTestTT tests_Generator
    putStrLn "==== Rodando testes Validation ===="
    _ <- runTestTT tests_Validation

    return ()
