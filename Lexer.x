{   
module Lexer(Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-z]

$white = [\x20\x09\x0A\x0D\x0C\x0B]

tokens:-

$white+             ;

0     {\s -> (TokenInt(read s))}
[1-9]$digit* {\s -> (TokenInt(read s)) }
\-[1-9]$digit* {\s -> (TokenInt(read s))}
\#t {\_ -> (TokenBool True)}
\#f {\_ -> (TokenBool False)}
\(  {\_-> TokenParenL}
\)  {\_ -> TokenParenR}
\[  {\_ -> TokenBracketL}
\]  {\_ -> TokenBracketR}
\,  {\_ -> TokenComma}
\+  {\_ -> TokenSuma}
\-  {\_ -> TokenResta}
\*  {\_-> TokenMult}
\/  {\_ -> TokenDiv}
\==  {\_ -> TokenEq}
\<  {\_ -> TokenMenor}
\>  {\_ -> TokenMayor}
\<= {\_ -> TokenMenorEq}
\>= {\_ -> TokenMayorEq}
\!= {\_ -> TokenDiff}

not {\_ -> TokenNot}
and {\_ -> TokenAnd}
or {\_ -> TokenOr}
if0 {\_ -> TokenIf0}
if {\_ -> TokenIf}
cond {\_ -> TokenCond}
let {\_ -> TokenLet}
let\* {\_ -> TokenLetE}
fst {\_ -> TokenFst}
snd {\_ -> TokenSnd}
head {\_ -> TokenHead}
tail {\_ -> TokenTail}
null {\_-> TokenNull}
else {\_-> TokenElse}
lambda {\_-> TokenLambda}

[a-z]$alpha*   {\s -> TokenId s}


{
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
        |TokenElse
        |TokenLambda
        deriving(Show)
        
normalizeSpaces :: String -> String
normalizeSpaces = map(\c -> if isSpace c then '\x20' else c)

lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}