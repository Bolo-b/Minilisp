module Interp where
import Desugar

type Env = [(String, ASAValues)]
data Value = NumV Int
        | BoolV Bool
        | ClosureV [Value]
        | NullV 
        | Error String
        deriving(Show,Eq)
busca :: String -> DS -> Value
busca s MtDS = Error "Free variable: "
busca s2 (D s1 val sig_ds) = if s1==s2 then val else busca s2 sig_ds
interp :: DesuExp -> DS -> Value
interp (Num n) ds = NumV n
interp (Bool b) ds = BoolV b
interp (Id i) ds = busca i ds
--Recibe una expresion, un identificador, una expresion y devuelve una expresion
--Entonces sustituye en este arbol llamado tal por este valor
sust :: Exp-> String-> Exp-> Exp
sust(NumV n) var val= NumP n
sust(BoolV b) var val= BoolP b 
sust NullV var val = NullV
--Asumimos por el momento que se va a definir el IdV
sust(IdV s) var val =