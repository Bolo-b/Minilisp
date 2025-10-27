module Desugar (desugar) where
import Parser (Exp(..))

data DesuExp = NumD Int
 | Bool Bool
 | Null
 | Id 
 | Add DesuExp DesuExp
 deriving(Show,Eq)

desugar :: Exp -> DesuExp

desugar (NumP n) = (NumD n)
desugar (BoolP b) = (BoolD b)
desugar (IdP x) = (Id x)
desugar NullP = NullD



