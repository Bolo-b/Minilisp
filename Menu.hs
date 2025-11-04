module Menu where

import MiniLisp (run)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Control.Exception (try, SomeException)

menu :: IO ()
menu = do
    putStrLn "\n ==== MINILISP ===="
    putStrLn "1) Agrega una expresión"
    putStrLn "2) Suma de los primeros n naturales"
    putStrLn "3) Factorial"
    putStrLn "4) Fibonacci"
    putStrLn "5) Map"
    putStrLn "6) Filter"
    putStrLn "7) Salir"
    putStr "Selecciona el número: "
    hFlush stdout
    opt <- getLine
    case opt of
        "1" -> agregaExpr
        "2" -> menuSumaNat
        "3" -> menuFact
        "4" -> menuFibo
        "5" -> menuMap
        "6" -> menuFilter
        "7" -> do putStrLn "Saliendo..."
                    exitSuccess
        _   -> do putStrLn "Inválido.\n"
                    menu

--Agregar expresión
agregaExpr :: IO ()
agregaExpr = do
    putStr "Minilisp>"
    hFlush stdout
    input <- getLine
    if input == ":q"
      then menu
      else do
        result <- try (return (run input)) :: IO (Either SomeException String)
        case result of
            Left ex  -> putStrLn ("Error: " ++ show ex)
            Right val -> putStrLn val
        agregaExpr

--Suma de los primeros n naturales
menuSumaNat :: IO ()
menuSumaNat = do
    putStr "n = "
    hFlush stdout
    n <- getLine
    let prog = "(letrec ((suma (lambda(n)(if (== n 0) 0 (+ n (suma (- n 1))))))) (suma " ++ n ++ "))"
    printResult prog

--Factorial
menuFact :: IO ()
menuFact = do
    putStr "n = "
    hFlush stdout
    n <- getLine
    let prog = "(letrec ((fact (lambda(n) (if (== n 0) 1 (* n (fact (- n 1))))))) (fact " ++ n ++" ))"
    printResult
