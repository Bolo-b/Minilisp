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
 | Lambda DesuExp DesuExp
 | App DesuExp DesuExp
 | Pair DesuExp DesuExp
 | Fst DesuExp
 | Snd DesuExp
 | HeadL DesuExp
 | TailL DesuExp
 deriving(Show,Eq)

desugar :: Exp -> DesuExp

desugar (NumP n) = (Num n)
desugar (BoolP b) = (Bool b)
desugar (IdP x) = (Id x)
desugar NullP = Null

desugar (AddP (ParamNumP p1 p2) e1) = Add (desugar(AddP p1 p2)) (desugar e1)
desugar (AddP e1 e2) = Add (desugar e1) (desugar e2)

desugar (SubP (ParamNumP (ParamNumP p1 p2) e)) = Add (desugar (SubP (ParamNumP p1 p2))) (desugar (SubP e))
desugar (SubP (ParamNumP p1 p2)) = Add (desugar p1) (desugar (SubP p2))
desugar (SubP e1) = Sub (Num 0) (desugar e1)

desugar (MultP (ParamNumP p1 p2) e1) = Mult (desugar(MultP p1 p2)) (desugar e1)
desugar (MultP e1 e2) = Mult (desugar e1) (desugar e2)

desugar (DivP (ParamNumP p1 p2) e1) = Div (desugar(DivP p1 p2)) (desugar e1)
desugar (DivP e1 e2) = Div (desugar e1) (desugar e2)

desugar (EqualsP e1 e2) = Equals (desugar e1) (desugar e2)
desugar (LessEP e1 e2) = LessE (desugar e1) (desugar e2)
desugar (GreatEP e1 e2) = GreatE (desugar e1) (desugar e2)
desugar (LessP e1 e2) = Less (desugar e1) (desugar e2)
desugar (GreatP e1 e2) = Great (desugar e1) (desugar e2)
desugar (Diff e1 e2) = DiffD (desugar e1) (desugar e2)

desugar (BNotP e) = Not (desugar e)
desugar (BAndP e1 e2) = And (desugar e1) (desugar e2)
desugar (BOrP e1 e2) = Or (desugar e1) (desugar e2)

desugar (IfP c t e) = If (desugar c) (desugar t) (desugar e)
desugar (CondP [(c, t)] e) = If (desugar c) (desugar t) (desugar e)
desugar (CondP ((c, t):xs) e) = If (desugar c) (desugar t) (desugar (CondP xs e))

desugar (FunP [(IdP i, v)] e) = App (Lambda (Id i) (desugar e)) (desugar v)
desugar (FunP ((IdP i, v):xs) e) = App (Lambda (Id i) (desugar (FunP xs e) )) (desugar v)

desugar (LambdaP (IdP i) e) = Lambda (Id i) (desugar e)
desugar (LambdaP (ParamIdP p1 (IdP i)) e) = desugar (LambdaP p1 (LambdaP (IdP i) e))

desugar (AppP f v) = App (desugar f) (desugar v)

desugar (HeadLP e) = HeadL (desugar e)
desugar (TailLP e) = TailL (desugar e)

desugar (PairP e1 e2) = Pair (desugar e1) (desugar e2)
desugar (FstP e) = Fst (desugar e)
desugar (SndP e) = Snd (desugar e)
