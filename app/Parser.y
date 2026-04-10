{
module Parser (logicParser) where
import Lexer
import Syntax
}

%name logicParser
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar $$ }
    not     { TokenNeg }
    and     { TokenConj }
    or     { TokenDisj }
    lpar     { TokenLParen }
    rpar     { TokenRParen }

-- Definición de precedencia (de menor a mayor)
%left or
%left and
%right not

%%

Exp : var               { Var $1 }
    | not Exp           { Neg $2 }
    | Exp and Exp       { Conj $1 $3 }
    | Exp or Exp       { Disj $1 $3 }
    | lpar Exp rpar       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Error de sintaxis: Revisar la expresión de entrada."
}
