{
    module Parser (parse, Expr(...)) where
    import Lexer (Token(..))
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
    '='         { TokenEq }
    '<'         { TokenMenor }
    '>'         { TokenMayor }
    '<='        { TokenMenorEq }
    '>='        { TokenMayorEq }
    '!='        { TokenDiff }

    not         { TokenNot }
    and         { TokenAnd }
    or          { TokenOr }

    if          { TokenIf }
    cond        { TokenCond }

    let         { TokenLet }
    let*        { TokenLetE }

    fst         { TokenFst }
    snd         { TokenSnd }
    head        { TokenHead }
    tail        { TokenTail }

    lambda      { TokenLambda }

%%

Expr : id                                { IdP $1 }
    | int                               { NumP $1 }
    | bool                              { BoolP $1 }
    | '(' '+' Param Param ')'           { AddP $3 $4 }
    | '(' '-' Param Param ')'           { SubP "-" $3 $4 }
    | '(' '*' Param Param ')'           { MultP "*" $3 $4 }
    | '(' '/' Param Param ')'           { DivP "/" $3 $4 }
    | '(' '=' Param Param ')'           { EqualsP "=" $3 $4 }
    | '(' '<' Param Param ')'           { LessEP "<" $3 $4 }
    | '(' '>' Param Param ')'           { GreatEP ">" $3 $4 }
    | '(' '<=' Param Param ')'          { LessP "<=" $3 $4 }
    | '(' '>=' Param Param ')'          { GreatP ">=" $3 $4 }
    | '(' '!=' Param Param ')'          { Diff "!=" $3 $4 }
    | '(' not Exp ')'                   { BNotP "not" $3 }
    | '(' and Exp Exp ')'               { BAndP "and" $3 $4 }
    | '(' or Exp Exp ')'                { BOrP "or" $3 $4 }
    | '(' if Exp Exp Exp ')'            { IfP $3 $4 $5 }
    | '(' cond Claus '[' else Exp ']' ')'             { CondP $3 $6}
--    | '(' let '(' SustList ')' '(' Exp ')' ')'   { ELet $4 $7 }
--    | '(' let* '(' SustList ')' '(' Exp ')' ')'  { ELetStar $4 $7 }
    | '(' lambda '(' Param Exp ')' Param ')'     { LambdaP $4 $5 $7 }
    | '(' Exp ',' Exp ')'               { PairP $2 $4 }
    | '(' fst Exp ')'                   { FirstP "fst" $3 }
    | '(' snd Exp ')'                   { FirstP "snd" $3 }
    | '[' List ']'                      { ListP $2 }
    | '(' head Exp ')'                  { HeadLP "head" $3 }
    | '(' tail Exp ')'                  { TailLP "tail" $3 }
    | null                              { NullP }

Param : id                              { IdP $1 }
    | int                               { NumP $1 }
    | bool                              { BoolP $1 }
    | Param int                         { ParamIntP $1 (EInt $2) }
    | Param bool                        { ParamBoolP $1 (EBool $2) }
    | Param id                          { ParamIdP $1 (EId $2) }
    | Exp                               { $1 }

Claus
    : '[' '(' Exp ')' '(' Exp ')' ']'   { [( $3, $6 )] }
    | Claus '[' '(' Exp ')' '(' Exp ')' ']'  { $1 ++ [($4, $7)] }


List : Expr                        { [$1] }
    | Expr ',' ExprList           { $1 : $3 }
