module Interp where
import Desugar (desugar, DesuExp(..))

type Env = [(String, Value)]

data Value = NumV Int
        | BoolV Bool
        | ClosureV [Value]
        | ListV [Value]
        | PairV (Value, Value)
        | Error String
        deriving(Show,Eq)

busca :: String -> Env -> Value
busca id [] = Error ("Variable libre: " ++ id)
busca id ((i,v):xs) = if id == i then v else busca id xs

interp :: DesuExp -> Env -> Value
interp (NumD n) _ = NumV n
interp (Bool b) _ = BoolV b
interp (Id i) e = busca i e
 esValor::Exp -> Bool
esValor (Num _) = True
esValor (Bool _) = True
esValor Null = True
esValor (Lambda _ _)= True
esValor(Pair e1 e2)= (esValor e1)&&(esValor e2)
esValor _= False
eval::Exp-> Exp
eval e
        |esValor e=e
        |otherwise = eval(Bstep e)
