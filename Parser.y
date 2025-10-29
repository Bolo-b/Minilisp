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
    letrec      { TokenLet }

    fst         { TokenFst }
    snd         { TokenSnd }
    head        { TokenHead }
    tail        { TokenTail }

    lambda      { TokenLambda }

%%

Exp : id                                { IdP $1 }
    | int                               { NumP $1 }
    | bool                              { BoolP $1 }
    | '(' '+' Param Param ')'           { AddP $3 $4 }
    | '(' '-' Param ')'                 { SubP $3 }
    | '(' '*' Param Param ')'           { MultP $3 $4 }
    | '(' '/' Param Param ')'           { DivP $3 $4 }
    | '(' '==' Param Param ')'          { EqualsP $3 $4 }
    | '(' '<' Param Param ')'           { LessEP $3 $4 }
    | '(' '>' Param Param ')'           { GreatEP $3 $4 }
    | '(' '<=' Param Param ')'          { LessP $3 $4 }
    | '(' '>=' Param Param ')'          { GreatP $3 $4 }
    | '(' '!=' Param Param ')'          { Diff $3 $4 }
    | '(' not Exp ')'                   { BNotP $3 }
    | '(' and Exp Exp ')'               { BAndP $3 $4 }
    | '(' or Exp Exp ')'                { BOrP $3 $4 }
    | '(' if Exp Exp Exp ')'            { IfP $3 $4 $5 }
    | '(' cond Claus '[' else Exp ']' ')'             { CondP $3 $6}

    | '(' let '(' Sust ')' Exp ')'   { FunP $4 $6 }
    | '(' 'let*' '(' Sust ')' Exp ')'  { FunPE $4 $6 }
    | '(' letrec '(' Sust ')' Exp ')'   { FunRecP $4 $6 }

    | '(' lambda '(' Param ')' Exp ')'     { LambdaP $4 $6 }
    | '(' Exp Param ')'                 { AppP $2 $3 } --Aplicación de función variádica
    | '(' Exp ',' Exp ')'               { PairP $2 $4 }
    | '(' fst Exp ')'                   { FstP $3 }
    | '(' snd Exp ')'                   { SndP $3 }
    | '[' List ']'                      { ListP $2 }
    | '[' ']'        { ListP [] }
    | '(' head Exp ')'                  { HeadLP $3 }
    | '(' tail Exp ')'                  { TailLP $3 }
    | null                              { NullP }

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

Sust
    : '(' Exp Exp ')'         { [($2, $3)] }
    | Sust '(' Exp Exp ')'   { $1 ++ [($3, $4)] }



List : Exp                        { [$1] }
    | Exp ',' List           { $1 : $3 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = NumP Int
            | BoolP Bool
            | IdP String
            | NullP
            | AddP Exp Exp
            | SubP Exp
            | MultP Exp Exp
            | DivP Exp Exp
            | EqualsP Exp Exp
            | LessEP Exp Exp
            | GreatEP Exp Exp
            | LessP Exp Exp
            | GreatP Exp Exp
            | Diff Exp Exp
            | BNotP Exp
            | BAndP Exp Exp
            | BOrP Exp Exp
            | IfP Exp Exp Exp
            | CondP [(Exp, Exp)] Exp
            | FunP [(Exp, Exp)] Exp
            | FunPE [(Exp, Exp)] Exp
            | FunRecP [(Exp, Exp)] Exp
--let y letrec
            | LambdaP Exp Exp
            | AppP Exp Exp
            | ParamNumP (Exp) Exp
            | ParamBoolP (Exp) Exp
            | ParamIdP (Exp) Exp
            | PairP Exp Exp
            | FstP Exp
            | SndP Exp
            | ListP [Exp]
            | HeadLP Exp
            | TailLP Exp
            deriving (Show, Eq)
}