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

Bstep:: Exp-> Exp
Bstep e | esValor e=e
Bstep (Id e) = error("Error")
Bstep (Add (Num n1) (Num))= Num(n1+ n2)
Bstep (Add (Num n1) e2)= Add (Num n1) (Bstep e2)
Bstep (Add e1 e2) = Add(Bstep e1)e2

Bstep (Sub (Num n1)(Num n2))= Num(n1-n2)
Bstep (Sub (Num n1) n2)= Sub (Num n1)(Bstep n2)
Bstep (Sub e1 e2)= Sub (Bstep e1)e2

Bstep (Mult (Num n1)(Num n2))= Num(n1*n2)
Bstep (Mult (Num n1) n2)= Mult (Num n1)(Bstep n2)
Bstep (Mult e1 e2)= Mult (Bstep e1)e2

BStep (Div (Num n1) (Num 0)) = error"No se puede dividir por cero"
BStep (Div (Num n1) (Num n2)) = Num (n1 `Div` n2)
BStep (Div (Num n1) e2) = Div (Num n1) (BStep e2)
BStep (Div e1 e2) = Div (BStep e1) e2

Bstep (Sub (Num n1)(Num n2))= Num(n1-n2)
Bstep (Sub (Num n1) n2)= Sub (Num n1)(Bstep n2)
Bstep (Sub e1 e2)= Sub (Bstep e1)e2

Bstep (Equals e1 e2)
        |not (esValor e1)= Equals(Bstep e1) e2
        | esValor && not (esValor e2)= Equals e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1==n2)
Bstep (LessE e1 e2)
        |not (esValor e1)= LessE(Bstep e1) e2
        | esValor && not (esValor e2)=LessE e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1<n2)
Bstep (GreatE e1 e2)
        |not (esValor e1)= GreatE(Bstep e1) e2
        | esValor && not (esValor e2)= GreatE e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1>n2)
Bstep (Less e1 e2)
        |not (esValor e1)= Less(Bstep e1) e2
        | esValor && not (esValor e2)= Less e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1<=n2)
Bstep (Great e1 e2)
        |not (esValor e1)= Great(Bstep e1) e2
        | esValor && not (esValor e2)= Great e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1>=n2)
Bstep (DiffD e1 e2)
        |not (esValor e1)= DiffD(Bstep e1) e2
        | esValor && not (esValor e2)= DiffD e1(Bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1!=n2)
Bstep(Not(Bool b))= Bool(not b)
Bstep(Not e)= Not(Bstep e)
Bstep(And (Bool False)_)= Bool False
Bstep(And (Bool True)e2)= e2
Bstep(And e1 e2)= And (Bstep e1) e2
Bstep(Or(Bool True)_)= Or True
Bstep(Or (Bool True)e2)= e2
Bstep(Or e1 e2)= Or(Bstep e1) e2
Bstep(If(Bool True)t _)= t
Bstep(If(Bool False)_ e)= e
BStep(If c t e)= If(Bstep c) t e
Bstep(App (Lambda p b)a)
        |esValor a= sust b p a
        |otherwise =App(Lambda p b) (Bstep a)
Bstep(App f a)= App(BStep f)a
Bstep(Pair e1 e2)
        |not (esValor e1)= Pair(esValor e1 )e2
        |esValor e1 && not (esValor e2 )= Pair e1 (Bstep e2)
        |otherwise =e
Bstep(Fst(Pair v1 v2)) | esValor(Pair v1 v2 )=v1
Bstep(Fst e)= Fst(Bstep e)
Bstep(Snd(Pair v1 v2)) | esValor(Pairv1 v2)= v2
Bstep(Snd e )= Snd(BStep e)
--Por si hay un error
Bstep e = error("Bstep fallo en la implementacion de"++ show e)


--Recibe una expresion, un identificador, una expresion y devuelve una expresion
--Entonces sustituye en este arbol llamado tal por este valor
sust :: Exp-> String-> Exp-> Exp
sust(Num n) _  _= Num n
sust(Bool b) _  _= Bool b 
sust Null _  _ = Null
sust(Id s) var val = if s == var then val else (Id s)
sust(Add e1 e2) var val= Add (sust e1 var val ) (sust e2 var val)
sust(Sub e1 e2) var val = Sub(sust e1 var val) (sust e2 var val)
sust(Mult e1 e2) var val =Mult(sust e1 var val) (sust e2 var val)
sust(Div e1 e2) var val = Div(sust e1 var val) (sust e2 var val)
sust(Equals e1 e2) var val = Equals(sust e1 var val)(sust e2 var val)
sust(LessE e1 e2) var val = LessE(sust e1 var val)(sust e2 var val)
sust(Great e1 e2) var val =  Great sust(sust e1 var val)( sust e2 var val)
sust(DiffD e1 e2) var val = DiffD(sust e1 var val)(sust e2 var val)
sust(Not e1) var val = Not(e1 var val)
sust(And e1 e2) var val= And(sust e1 var val)(sust e2 var val)
sust(Or e1 e2) var val= Or(sust e1 var val)(sust e2 var val)
sust(Sub e) var val = Sub (sust e var val)
sust(If c t e) var val= If (sust c var val) (sust t var val) (sust e var val)
sust(App f a) var val= Add (sust f var val) (sust a var val)
sust(Pair e1 e2 ) var val = Pair(sust e1 var val) (sust e2 var val)
sust(Fst e) var val = Fst (sust e var val)
sust(Snd e) var val = Snd (sust e var val)
sust(HeadL e) var val = HeadL(sust e var val)
sust(TailL e) var val = TailL(sust e var val)
sust(Lambda p c) i v =
        if i == p
        then Lambda p c
        else Lambda (sust c i v)
sust(App f a) i v = App (sust f i v)(sust a i v)
--Para ver que error sale
sust e _ _ = error ("Fallo o falta en la implementacion de:"++ show e)
--Dudas con el Value
