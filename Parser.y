{
module Parser (parse, Exp(..)) where
import Lexer (lexer, Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    int         { TokenInt $$ }
    bool        { TokenBool $$ }
    id          { TokenId $$ }
    null        { TokenNull }
    '('         { TokenParenL }
    ')'         { TokenParenR }
    '['         { TokenBracketL }
    ']'         { TokenBracketR }
    ','         { TokenComma }

    '+'         { TokenSuma }
    '-'         { TokenResta }
    '*'         { TokenMult }
    '/'         { TokenDiv }
    '=='         { TokenEq }
    '<'         { TokenMenor }
    '>'         { TokenMayor }
    '<='        { TokenMenorEq }
    '>='        { TokenMayorEq }
    '!='        { TokenDiff }

    not         { TokenNot }
    and         { TokenAnd }
    or          { TokenOr }

    if          { TokenIf }
    else        { TokenElse}
    cond        { TokenCond }

    let         { TokenLet }
    'let*'        { TokenLetE }
    letrec      { TokenLetRec }

    fst         { TokenFst }
    snd         { TokenSnd }
    head        { TokenHead }
    tail        { TokenTail }

    lambda      { TokenLambda }

%%

Exp : id                                { IdP $1 }
    | int                               { NumP $1 }
    | bool                              { BoolP $1 }
    | null                              {NullP}
    | '(' '+' ExpList2 ')'           { AddP $3}
    | '(' '-' ExpList2 ')'                 { SubP $3 }
    | '(' '*' ExpList2 ')'           { MultP $3 }
    | '(' '/' ExpList2 ')'           { DivP $3 }
    | '(' '==' ExpList2 ')'          { EqualsP $3 }
    | '(' '<' ExpList2 ')'           { LessEP $3 }
    | '(' '>' ExpList2 ')'           { GreatEP $3 }
    | '(' '<=' ExpList2 ')'          { LessP $3 }
    | '(' '>=' ExpList2 ')'          { GreatP $3 }
    | '(' '!=' Param Param ')'          { Diff $3 $4 }
    | '(' not Exp ')'                   { BNotP $3 }
    | '(' '-' Exp ')'                   {NegP $3}
    | '(' and Exp Exp ')'               { BAndP $3 $4 }
    | '(' or Exp Exp ')'                { BOrP $3 $4 }
    | '(' if Exp Exp Exp ')'            { IfP $3 $4 $5 }
    | '(' cond Claus '[' else Exp ']' ')'             { CondP $3 $6}

    | '(' let '(' SustList ')' Exp ')'   { FunP $4 $6 }
    | '(' 'let*' '(' Sust ')' Exp ')'  { FunPE $4 $6 }
    | '(' letrec '(' Sust ')' Exp ')'   { FunRecP $4 $6 }

    | '(' lambda '(' IdList ')' Exp ')'     { LambdaP $4 $6 }
    | '(' Exp ExpList ')'                 { AppP $2 $3 } --Aplicación de función variádica
    | '(' Exp ',' Exp ')'               { PairP $2 $4 }
    | '(' fst Exp ')'                   { FstP $3 }
    | '(' snd Exp ')'                   { SndP $3 }
    | '[' List ']'                      { ListP $2 }
    | '[' ']'        { ListP [] }
    | '(' head Exp ')'                  { HeadLP $3 }
    | '(' tail Exp ')'                  { TailLP $3 }

Param : id                              { IdP $1 }
    | int                               { NumP $1 }
    | bool                              { BoolP $1 }
    | Param int                         { ParamNumP $1 (NumP $2) }
    | Param bool                        { ParamBoolP $1 (BoolP $2) }
    | Param id                          { ParamIdP $1 (IdP $2) }
    | Exp                               { $1 }

Claus
    : '[' Exp Exp ']'         { [($2, $3)] }
    | Claus '[' Exp Exp ']'   { $1 ++ [($3, $4)] }

SustBinding: '(' id Exp ')'     { ($2,$3) }
SustList:   SustBinding     { [$1] }
        |   SustList SustBinding    { $1 ++ [$2] }



List : Exp                        { [$1] }
    | Exp ',' List           { $1 : $3 }

ExpList : Exp                 { [$1] }
        | Exp ExpList   { $1: $2}
ExpList2 : Exp ExpList  { $1: $2}
IdList : id {[$1]}
        | id IdList { $1 : $2}     
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = NumP Int
            | BoolP Bool
            | IdP String
            | NullP
            | AddP [Exp]
            | SubP [Exp]
            | NegP Exp
            | MultP [Exp]
            | DivP [Exp]
            | EqualsP [Exp]
            | LessEP [Exp]
            | GreatEP [Exp]
            | LessP [Exp]
            | GreatP [Exp]
            | Diff Exp Exp
            | BNotP Exp
            | BAndP Exp Exp
            | BOrP Exp Exp
            | IfP Exp Exp Exp
            | CondP [(Exp, Exp)] Exp
            | FunP [(String, Exp)] Exp
            | FunPE [(String, Exp)] Exp
            | FunRecP [(String, Exp)] Exp
--let y letrec
            | LambdaP [String] Exp
            | AppP Exp [Exp]
            | PairP Exp Exp
            | FstP Exp
            | SndP Exp
            | ListP [Exp]
            | HeadLP Exp
            | TailLP Exp
            deriving (Show, Eq)
}