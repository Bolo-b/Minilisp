module Menu where

import Interp (runString)
import System.IO (hFlush, stdout)
import GHC.IO.FD (stdout)
import GHC.ResponseFile (expandResponse)

menu :: IO
menu = do
    putStrLn "----------MINILISP----------"
    menu

menu :: IO
menu = do
    putStrLn "\n ----MENU----"
    putStrLn "1) Agrega una expresión"
    putStrLn "2) Suma de los primeros n naturales"
    putStrLn "3) Factorial"
    putStrLn "4) Fibonacci"
    putStrLn "5) Map"
    putStrLn "6) Filter"
    putStrLn "7) Casos de prueba"
    putStrLn "8) Salir"
    putStr "Selecciona el número: " >> hFlush stdout
    opt <- getLine
    case opt of
        "1" -> do
            putStrLn "Escribe una expresión en sintaxis concreta: "
            putStr ">>> " hFlush stdout
            expr <- getLine
            putStrLn "\n --- Resultado ---"
            runString expandResponse
            menu
        "2" -> menuSumaNat
        "3" -> menuFact
        "4" -> menuFibo
        "5" -> menuMap
        "6" -> menuFilter
        "7" -> menuCasos
        "8" -> putStrLn "Salir..."
        _   -> putStrLn "Inválido.\n" >> menu