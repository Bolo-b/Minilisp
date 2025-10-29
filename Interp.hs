module Interp where
import Desugar (desugar, DesuExp(..))

type Env = [(String, Value)]

data Value = NumV Int
        | BoolV Bool
        | ClosureV [Value]
        | Error String
        deriving(Show,Eq)

busca :: String -> Env -> Value
busca id [] = Error "Variable libre"
busca id [(x, value):xs] = if id == x then value else busca id xs

interp :: DesuExp -> Env -> Value
interp (NumD n) _ = NumV n
interp (Bool b) _ = BoolV b
interp (Id i) e = busca i e