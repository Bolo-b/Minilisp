module Desugar (desugar) where
import Parser (Exp(..))

data DesuExp = NumD Int
 | Bool Bool
 | Null
 | Id String
 | Add DesuExp DesuExp
 | Sub DesuExp DesuExp
 | Mult DesuExp DesuExp
 | Div DesuExp DesuExp
 deriving(Show,Eq)

desugar :: Exp -> DesuExp

desugar (NumP n) = (NumD n)
desugar (BoolP b) = (BoolD b)
desugar (IdP x) = (Id x)
desugar NullP = NullD

desugar (AddP (ParamNumP p1 p2) e1) = Add (Add (desugar p1) (desugar p2)) (desugar e1)
desugar (AddP e1 e2) = Add (desugar e1) (desugar e2)
desugar (SubP (ParamNumP p1 p2) e1) = Sub (Sub (desugar p1) (desugar p2)) (desugar e1)
desugar (SubP e1 e2) = Sub (desugar e1) (desugar e2)
desugar (MultP (ParamNumP p1 p2) e1) = Mult (Mult (desugar p1) (desugar p2)) (desugar e1)
desugar (MultP e1 e2) = Mult (desugar e1) (desugar e2)
desugar (DivP (ParamNumP p1 p2) e1) = Div (Div (desugar p1) (desugar p2)) (desugar e1)
desugar (DivP e1 e2) = Div (desugar e1) (desugar e2)



