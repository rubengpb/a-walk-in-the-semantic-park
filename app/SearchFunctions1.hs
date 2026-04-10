module SearchFunctions1 where
import Syntax

data PotencialRedex =
    PrNeg Term
  | PrConj Term Term
  | PrDisj Term Term
  deriving (Eq, Show, Read)

data Found =
  Val TermNF
  | PotRed PotencialRedex
  deriving (Eq, Show, Read)

searchTermNeg :: Term -> Found
searchTermNeg (Var x) = Val (NegVarNF x)
searchTermNeg (Neg t) = PotRed (PrNeg t)
searchTermNeg (Conj t1 t2) = PotRed (PrConj t1 t2)
searchTermNeg (Disj t1 t2) = PotRed (PrDisj t1 t2)

searchTerm :: Term -> Found
searchTerm (Var x) = Val (PosVarNF x)
searchTerm (Neg t) = searchTermNeg t
searchTerm (Conj t1 t2) =
    case searchTerm t1 of
      Val v1 -> case searchTerm t2 of
                  Val v2 -> Val (ConjNF v1 v2)
                  PotRed pr -> PotRed pr
      PotRed pr -> PotRed pr
searchTerm (Disj t1 t2) =
    case searchTerm t1 of
      Val v1 -> case searchTerm t2 of
                  Val v2 -> Val (DisjNF v1 v2)
                  PotRed pr -> PotRed pr
      PotRed pr -> PotRed pr
