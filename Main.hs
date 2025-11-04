module Main where

import Menu (menu)

main:: IO()
main = do
    putStrLn "Minilisp>"
    putStrLn "Escribe ':q' para salir"
    menu
