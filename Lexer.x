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
            |TokenEq
            |TokenAdd
            |TokenSub
            |TokenSqrt
            |TokenExpt
            |TokenMenor
            |TokenMayor
            |TokenMenorEq
            |TokenMayorEq
            |TokenDiff
            |TokenNot
            |TokenAnd
            |TokenOr
            |TokenIf0
            |TokenIf
            |TokenCond
            |TokenLet
            |TokenLetE
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
$white = [\x20\x09\x0A\x0D\x0C\x0B]
tokens:-
$white+             
0     {\s -> return(TokenInt(read s))}
[1-9]$digit* {\s ->return (TokenInt(read s)) }
\-[1-9]$digit* {\s -> return (TokenInt(read s))}
\#t {\_ -> return (TokenBool True)}
\#f {\_ -> return (TokenBool False)}
\(  {\_-> return TokenParenL}
\)  {\_ ->return TokenParenR}
\[  {\_ -> return TokenBracketL}
\]  {\_ -> return TokenBracketR}
\,  {\_ -> return TokenComma}
\+  {\_ -> return TokenSuma}
\*  {\_-> return TokenMult}
\/  {\_ -> return TokenDiv}
\=  {\_ ->return TokenEq}
\<  {\_ ->return TokenMenor}
\>  {\_ ->return TokenMayor}
\<= {\_ -> return TokenMenorEq}
\>= {\_ -> return TokenMayorEq}
\!= {\_ -> return TokenDiff}

add {\_ -> return TokenAdd}
sub {\_ ->return TokenSub}
sqrt {\_ ->return TokenSqrt}
expt {\_ ->return TokenExpt}

not {\_ -> return TokenNot}
and {\_ -> return TokenAnd}
or {\_ -> return TokenOr}
if0 {\_ ->return TokenIf0}
if {\_ -> return TokenIf}
cond {\_ -> return TokenCond}
let {\_ -> return TokenLet}
let\* {\_ -> return TokenLetE}
fst {\_ -> return TokenFst}
snd {\_ -> return TokenSnd}
head {\_ -> return TokenHead}
tail {\_ -> return TokenTail}
null {\_-> return TokenNull}
lambda {\_-> return TokenLambda}
