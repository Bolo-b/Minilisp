module MiniLisp where
import Interp

saca :: ASAFinal -> String
saca (NumF n) = n
saca (BoolF b)
  | b == True = "#t"
  | otherwise = "#f"
saca (ClosureV p c e) = "lambda(" + p + " " + c + ")"
