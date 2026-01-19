module Main where
import UI (menu)

main :: IO ()
main = do
    _ <- menu
    return ()