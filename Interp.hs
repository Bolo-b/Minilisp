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
interp (Num n) _ = NumV n
interp (Bool b) _ = BoolV b
interp (Id i) e = busca i e

esValor:: DesuExp -> Bool
esValor (Num _) = True
esValor (Bool _) = True
esValor Null = True
esValor (Lambda _ _)= True
esValor(Pair e1 e2)= esValor e1 && esValor e2
esValor _= False

eval::DesuExp -> DesuExp
eval e
        |esValor e=e
        |otherwise = eval(bstep e)

bstep:: DesuExp -> DesuExp
bstep e | esValor e=e
bstep (Id e) = error "Error"
bstep (Add (Num n1) (Num n2))= Num(n1+ n2)
bstep (Add (Num n1) e2)= Add (Num n1) (bstep e2)
bstep (Add e1 e2) = Add(bstep e1)e2

bstep (Sub (Num n1)(Num n2))= Num(n1-n2)
bstep (Sub (Num n1) n2)= Sub (Num n1)(bstep n2)
bstep (Sub e1 e2)= Sub (bstep e1)e2

bstep (Mult (Num n1)(Num n2))= Num(n1*n2)
bstep (Mult (Num n1) n2)= Mult (Num n1)(bstep n2)
bstep (Mult e1 e2)= Mult (bstep e1)e2

bstep (Div (Num n1) (Num 0)) = error"No se puede dividir por cero"
bstep (Div (Num n1) (Num n2)) = Num (n1 `div` n2)
bstep (Div (Num n1) e2) = Div (Num n1) (bstep e2)
bstep (Div e1 e2) = Div (bstep e1) e2

bstep (Equals e1 e2)
        |not (esValor e1)= Equals(bstep e1) e2
        | (esValor e1) && not (esValor e2) = Equals e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1==n2)
bstep (LessE e1 e2)
        |not (esValor e1)= LessE(bstep e1) e2
        | (esValor e1) && not (esValor e2)=LessE e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1<n2)
bstep (GreatE e1 e2)
        |not (esValor e1)= GreatE(bstep e1) e2
        | (esValor e1) && not (esValor e2)= GreatE e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1>n2)
bstep (Less e1 e2)
        |not (esValor e1)= Less(bstep e1) e2
        | (esValor e1) && not (esValor e2)= Less e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1<=n2)
bstep (Great e1 e2)
        |not (esValor e1)= Great(bstep e1) e2
        | (esValor e1) && not (esValor e2)= Great e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1>=n2)
bstep (DiffD e1 e2)
        |not (esValor e1)= DiffD(bstep e1) e2
        | (esValor e1) && not (esValor e2)= DiffD e1(bstep e2)
        | otherwise = case (e1, e2) of
                (Num n1, Num n2)-> Bool(n1 /= n2)
bstep(Not(Bool b))= Bool(not b)
bstep(Not e)= Not(bstep e)
bstep(And (Bool False)_)= Bool False
bstep(And (Bool True)e2)= e2
bstep(And e1 e2)= And (bstep e1) e2
bstep(Or(Bool True)_)= Bool True
bstep(Or e1 e2)= Or(bstep e1) e2
bstep(If(Bool True)t _)= t
bstep(If(Bool False)_ e)= e
bstep(If c t e)= If(bstep c) t e
bstep(App (Lambda p b)a)
        |esValor a = sust b p a
        |otherwise =App(Lambda p b) (bstep a)
bstep(App f a)= App(bstep f)a
bstep (Fix (Lambda x e)) = subst (x, Fix (Lambda x e)) e
bstep (Fix e) = Fix (bstep e)
bstep(Pair e1 e2)
        |not (esValor e1) = Pair (bstep e1 ) e2
        |esValor e1 && not (esValor e2 ) = Pair e1 (bstep e2)
        |otherwise = (Pair e1 e2)
bstep(Fst(Pair v1 v2)) | esValor(Pair v1 v2 )=v1
bstep(Fst e)= Fst(bstep e)
bstep(Snd(Pair v1 v2)) | esValor(Pair v1 v2)= v2
bstep(Snd e )= Snd(bstep e)
--Por si hay un error
bstep e = error("bstep fallo en la implementacion de"++ show e)


--Recibe una expresion, un identificador, una expresion y devuelve una expresion
--Entonces sustituye en este arbol llamado tal por este valor
sust :: DesuExp -> String -> DesuExp -> DesuExp
sust(Num n) _  _= Num n
sust(Bool b) _  _= Bool b 
sust Null _  _ = Null
sust(Id s) var val = if s == var then val else Id s
sust(Add e1 e2) var val= Add (sust e1 var val ) (sust e2 var val)
sust(Sub e1 e2) var val = Sub(sust e1 var val) (sust e2 var val)
sust(Mult e1 e2) var val =Mult(sust e1 var val) (sust e2 var val)
sust(Div e1 e2) var val = Div(sust e1 var val) (sust e2 var val)
sust(Equals e1 e2) var val = Equals(sust e1 var val)(sust e2 var val)
sust(LessE e1 e2) var val = LessE(sust e1 var val)(sust e2 var val)
sust(Great e1 e2) var val =  Great (sust e1 var val)( sust e2 var val)
sust(DiffD e1 e2) var val = DiffD(sust e1 var val)(sust e2 var val)
sust(Not e1) var val = Not( sust e1 var val)
sust(And e1 e2) var val= And(sust e1 var val)(sust e2 var val)
sust(Or e1 e2) var val= Or(sust e1 var val)(sust e2 var val)

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
        else Lambda p (sust c i v)

--Para ver que error sale
sust e _ _ = error ("Fallo o falta en la implementacion de:"++ show e)
--Dudas con el Value
