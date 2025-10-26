module Desugar (desugar) where
import Parser (Expr(..))

desugar :: Expr -> Expr

desugar (IntP n) = IntP n
desugar (