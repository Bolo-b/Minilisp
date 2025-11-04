module MiniLisp where

import Interp(eval)
import Lexer(lexer)
import Desugar(desugar, DesuExp(..))
import Parser(parse)


run:: String-> String
run s = saca (eval(desugar(parse(lexer s))))

saca :: DesuExp -> String
saca (Num n) = show n
saca (Bool b) = if b then "#t" else "#f"
saca Null = "[]"
saca (Pair v1 v2) = "(" ++ (saca v1) ++ ", " ++ (saca v2) ++ ")"
saca (Lambda _ _) = "<funcion>"
saca (Fix e) = "<fix>"
saca (Id s) = s
saca e = "(Expresión compleja: " ++ (show e) ++ ")"
