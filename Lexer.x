{   
--Convierte una cadena de caracteres en una lista de tokens
module Lexer(Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"
--Definiciones para la clase de caracteres
$digit = 0-9
$alpha = [a-zA-Z]
--Caracteres en blanco 
$white = [\ \t\n\r\f\v]

--Reglas para los tokens
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
if {\_ -> TokenIf}
cond {\_ -> TokenCond}
let {\_ -> TokenLet}
let\* {\_ -> TokenLetE}
letrec {\_ -> TokenLetRec}
fst {\_ -> TokenFst}
snd {\_ -> TokenSnd}
head {\_ -> TokenHead}
tail {\_ -> TokenTail}
null {\_-> TokenNull}
else {\_-> TokenElse}
lambda {\_-> TokenLambda}

--Token para identificadores
[a-z]$alpha*   {\s -> TokenId s}

--Representa todos los tokens posibles
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
        |TokenLetRec
        |TokenFst
        |TokenSnd
        |TokenHead
        |TokenTail
        |TokenElse
        |TokenLambda
        deriving(Show)
--Funcion para normalizar espacios en blanco 
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map (\c -> if isSpace c then ' ' else c)
--Funcion del lexer
lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
