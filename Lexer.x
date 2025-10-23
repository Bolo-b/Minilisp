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
            deriving(Show)
            normalizeSpaces :: String -> String
            normalizeSpaces = map(\c -> if isSpace c then '\x20' else c)
            lexer :: String -> [Token]
            lexer = alexScanTokens . normalizeSpaces
}
%wrapper "basic"
$digit = 0-9
$alpha =[a-z]
\x20 = ' ' (space), \x09 = tab, \x0A = LF, \x0D = CR, \x0C = FF, \x0B = VT
%white =[\x20\x09\x0A\x0D\X0C\X0B]
tokens:-
$white+             
0     {\s -> return(TokenInt(read s))}
[1-9]$digit* {\s ->return (TokenInt(read s)) }
\-[1-9]$digit* {\s -> return (TokenInt(read s))}
\#t  {\_ -> return (TokenBool True)}
\#f  {\_ -> return (TokenBool False)}