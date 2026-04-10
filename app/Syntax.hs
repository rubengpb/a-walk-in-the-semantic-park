module Syntax where

data Term =
    Var String
  | Neg Term
  | Conj Term Term
  | Disj Term Term
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (Neg (Var s)) = "¬" ++ s
  show (Neg s) = "¬" ++ show s
  show (Conj t1 t2) = "(" ++ show t1 ++ " ∧ " ++ show t2 ++ ")"
  show (Disj t1 t2) = "(" ++ show t1 ++ " ∨ " ++ show t2 ++ ")"

termFoldr :: (String -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Term -> a
termFoldr var neg conj disj t =
  let visit (Var x)      = var x
      visit (Neg t')     = neg (visit t')
      visit (Conj t1 t2) = conj (visit t1) (visit t2)
      visit (Disj t1 t2) = disj (visit t1) (visit t2)
  in visit t

data TermNF =
    PosVarNF String
  | NegVarNF String
  | ConjNF TermNF TermNF
  | DisjNF TermNF TermNF
  deriving (Eq, Show, Read)

termNFFoldr :: (String -> a) -> (String -> a) -> (a -> a -> a) -> (a -> a -> a) -> TermNF -> a
termNFFoldr posvar negvar conj disj t =
  let visit (PosVarNF x)      = posvar x
      visit (NegVarNF x)     = negvar x
      visit (ConjNF t1 t2) = conj (visit t1) (visit t2)
      visit (DisjNF t1 t2) = disj (visit t1) (visit t2)
  in visit t

dualize :: TermNF -> TermNF
dualize = termNFFoldr NegVarNF PosVarNF DisjNF ConjNF
