{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "¬" | "~"                     { \_ -> TokenNeg }
  "∧" | "&"                     { \_ -> TokenConj }
  "∨" | "|"                     { \_ -> TokenDisj }
  "("                           { \_ -> TokenLParen }
  ")"                           { \_ -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenVar s }

{
data Token = TokenVar String
           | TokenNeg
           | TokenConj
           | TokenDisj
           | TokenLParen
           | TokenRParen
           deriving (Eq, Show)
}
