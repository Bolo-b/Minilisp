{
    module Parser (parse, Expr(...)) where
    import Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }