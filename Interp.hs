module Interp where
import Desugar

type Env = [(String, ASAValues)]
data Value = NumV Int
        | BoolV Bool
        | ClosureV [Value]
        | NullV
        | IdP String
        | AddP Exp Exp
        | SubP Exp Exp
        | MultP Exp Exp
        | DivP Exp Exp
        | EqualsP Exp Exp
        | LessEP Exp Exp
        | GreatP Exp Exp
        | Diff Exp Exp
        | BNotP Exp
        | BorP Exp Exp
        | IfP Exp Exp Exp
      --| LetP String Exp Exp
      --| LambdaP String Exp
        | AppP Exp Exp
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
sust(NumV n) _  _= NumP n
sust(BoolV b) _  _= BoolP b 
sust NullV _  _ = NullV
sust(IdP s) var val = if s == var then val else (IdP s)
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
sust(BOrP e1 e2) var val= BorP(sust e1 var val)(sust e2 var val)
sust(SubP e) var val = SubP (sust e var val)
sust(IfP c t e) var val= IfP (sust c var val) (sust t var val) (sust e var val)
sust(AppP f a) var val= AddP (sust f var val) (sust a var val)
sust(PairP e1 e2 ) var val = PairP(sust e1 var val) (sust e2 var val)
sust(FstP e) var val = FstP (sust e var val)
sust(SndP e) var val = SndP (sust e var val)
sust(ListP exps) var val = ListP --???
sust(HeadLP e) var val = HeadLP(sust e var val)
sust(TailLP e) var val = TailLP(sust e var val)
--Falta Lambda,FunP,FunRecP y Lista

--Para ver que error Sale
sust e _ _ = error ("Fallo o falta en la implementacion de:"++ show e)



evalPP:: 