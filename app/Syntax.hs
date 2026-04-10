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
