{   
    module Lexer(
        --Igual depende de la forma en que se implemento el parser
        Token(..), lexer
    )where
    --Empezamos Definiendo los tokens
data Token
            = TokenInt Int
            |TokenBool Bool
            |TokenId  String
            |TokenNull
            |TokenParenL
            |TokenParenR
            |TokenBracketR
            |TokenBracketL
            |TokenComma
            --Operaciones
            |TokenSuma
            |TokenResta
            |TokenMult
            |TokenDiv
            |TokenAdd
            |TokenSub
            |TokenSqrt
            |TokenExpt
            |TokenNot
            |TokenAnd
            |TokenOr
            |TokenIf0
            |TokenIf
            |TokenCond
            |TokenLet
            |TokenLet*
            |TokenFst
            |TokenSnd
            |TokenHead
            |TokenTail
            |TokenLambda
}
