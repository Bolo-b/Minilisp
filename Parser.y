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