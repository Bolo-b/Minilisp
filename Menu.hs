module Menu where

import MiniLisp (run)
import System.IO (hFlush, stdout, putStr)
import System.Exit (exitSuccess)
import Control.Exception (try, SomeException (SomeException))

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
        "7" -> do
            putStrLn "Saliendo..."
            exitSuccess
        _   -> do
            putStrLn "Inválido.\n"
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
    printResult prog

--Fibonacci
menuFibo :: IO ()
menuFibo = do
    putStr "n = "
    hFlush stdout
    n <- getLine
    let prog = "(letrec ((fib (lambda(n) (if (<= n 1) n (+ (fib (-n 1)) (fib (- n 2))))))) (fib " ++ n ++ "))"
    printResult prog

--Map
menuMap :: IO ()
menuMap = do
    putStrLn "Ejemplo: aplica (+1) a [1 2 3]"
    let prog = "(letrec ((map (lambda (f lst) (if (== lst []) [] (pair (f (fst lst)) (map f (snd lst))))))) (map (lambda (x) (+ x 1)) [1 2 3]))"
    printResult prog

--Filter
menuFilter :: IO ()
menuFilter = do
    putStrLn "Ejemplo: filtra elementos > 2 en [1 2 3 4]"
    let prog = "(letrec ((filter (lambda (p lst) (if (== lst []) [] (if (p (fst lst)) (pair (fst lst) (filter p (snd lst))) (filter p (snd lst))))))) (filter (lambda (x) (> x 2)) [1 2 3 4]))"
    printResult prog

--Muestra el resultado con printResult
printResult :: String -> IO ()
printResult prog = do
    result <- try (return (run prog)) :: IO (Either SomeException String)
    case result of
        Left ex -> putStrLn ("Error: " ++ show ex)
        Right val -> putStr ("Resultado: " ++ val)
    menu