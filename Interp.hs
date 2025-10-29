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
sust(IdV s) var val = if s== var then var else (IdV s)
sust(AddP e1 e2) var val= AddP (sust e1 var val ) (sust e2 var val)
sust(SubP e1 e2) var val = SubP(sust e1 var val) (sust e2 var val)
sust(MultP e1 e2) var val =MultP(sust e1 var val) (sust e2 var val)
sust(DivP e1 e2) var val = DivP(sust e1 var val) (sust e2 var val)
sust(EqualsP e1 e2) var val = EqualsP(sust e1 var val)(sust e2 var val)
sust(LessEP e1 e2) var val = LessEP(sust e1 var val)(sust e2 var val)
sust(GreatP e1 e2) var val =  GreatP sust(sust e1 var val)( sust e2 var val)
sust(Diff e1 e2) var val = Diff(sust e1 var val)(sust e2 var val)
sust(BNotP e1) var val = BNotP(e1 var val)
sust(BAndP e1 e2) var val= BAndP(sust e1 var val)(sust e2 var val)
sust(BOrP e1 e2)var val= BorP(sust e1 var val)(sust e2 var val)
--Falta If y aplicacion de funciones



evalPP:: 