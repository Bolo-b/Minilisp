module Desugar (desugar, DesuExp(..)) where
import Parser (parse, Exp(..))
import Lexer (lexer, Token(..))

data DesuExp = Num Int
 | Bool Bool
 | Null
 | Id String
 | Add DesuExp DesuExp
 | Sub DesuExp DesuExp
 | Mult DesuExp DesuExp
 | Div DesuExp DesuExp
 | Equals DesuExp DesuExp
 | LessE DesuExp DesuExp
 | GreatE DesuExp DesuExp
 | Less DesuExp DesuExp
 | Great DesuExp DesuExp
 | DiffD DesuExp DesuExp
 | Not DesuExp
 | And DesuExp DesuExp
 | Or DesuExp DesuExp
 | If DesuExp DesuExp DesuExp
 | Lambda String DesuExp
 | App DesuExp DesuExp
 | Pair DesuExp DesuExp
 | Fst DesuExp
 | Snd DesuExp
 | HeadL DesuExp
 | TailL DesuExp
 | Fix DesuExp
 deriving(Show,Eq)

desugar :: Exp -> DesuExp

desugar (NumP n) = (Num n)
desugar (BoolP b) = (Bool b)
desugar (IdP x) = (Id x)
desugar NullP = Null
desugar (NegP e)= Sub(Num 0)(desugar e)
--Se necesita hacer modificacion a lo que tenemos
desugar (AddP exps) = desugarVariadico Add exps

desugar (SubP exps) = desugarVariadico Sub exps

desugar (MultP exps) = desugarVariadico Mult exps

desugar (DivP exps) =  desugarVariadico Div exps

desugar (EqualsP exps) = desugarVariadico Equals exps
desugar (LessEP exps) = desugarVariadico LessE exps
desugar (GreatEP exps) = desugarVariadico GreatE exps
desugar (LessP exps) =  desugarVariadico Less exps
desugar (GreatP exps) = desugarVariadico Great exps
desugar (DiffP exps) = desugarVariadico DiffD exps

desugar (BNotP e) = Not (desugar e)
desugar (BAndP e1 e2) = And (desugar e1) (desugar e2)
desugar (BOrP e1 e2) = Or (desugar e1) (desugar e2)

desugar (IfP c t e) = If (desugar c) (desugar t) (desugar e)
desugar (CondP [] e) = desugar e
desugar (CondP ((c, t):xs) e) = If (desugar c) (desugar t) (desugar (CondP xs e))

desugar (FunP b body)=
    let
        vars = map fst b
        vals = map desugar (map snd b)
        lambda = desugar (LambdaP vars body)
    in
        foldl App lambda vals
desugar (FunPE [] e)= desugar e
desugar (FunPE(( i, v):xs) e)=
    App (Lambda i (desugar (FunPE xs e))) (desugar v)
desugar (FunRecP [] e) = desugar e
desugar (FunRecP ((var,val):xs)e)=
    App(Lambda var (desugar(FunRecP xs e))) (Fix (Lambda var (desugar val)))

--desugar (FunRecP [(IdP f, e1)] e) = App (Lambda (f) (desugar e)) (Fix (Lambda (f) (desugar e1)))
--desugar (FunRecP ((IdP f, e1):xs) e) = desugar (FunRecP xs (FunRecP [(IdP f, e1)] e))
--desugar (FunP [(IdP i, v)] e) = App (Lambda (i) (desugar e)) (desugar v)
--desugar (FunP ((IdP i, v):xs) e) = App (Lambda (i) (desugar (FunP xs e) )) (desugar v)
--desugar (LambdaP (IdP i) e) = Lambda (Id i) (desugar e)

desugar (LambdaP [p] body) = Lambda p (desugar body)
desugar (LambdaP (p:ps) body) = Lambda p (desugar (LambdaP ps body))
desugar (LambdaP [] _)= error"Lambda sin parametros"
desugar (AppP f args) =  foldl App (desugar f) ( map desugar args)
--Primeras ideas par List
desugar (ListP [])= Null
desugar (ListP(x:cs ))= Pair (desugar x) (desugar(ListP cs))
desugar (HeadLP e) = Fst (desugar e)
desugar (TailLP e) = Snd (desugar e) 

desugar (PairP e1 e2) = Pair (desugar e1) (desugar e2)
desugar (FstP e) = Fst (desugar e)
desugar (SndP e) = Snd (desugar e)
desugarVariadico:: (DesuExp -> DesuExp-> DesuExp)-> [Exp]-> DesuExp
desugarVariadico op exps =
    let desugar_exps = map desugar exps
    in case desugar_exps of
        (e1:e2:es)-> foldl op (op e1 e2) es
        [e]->e
        []-> error "Fallo el desugarVariadico"
