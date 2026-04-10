module SearchFunctions2 where
import Syntax
import SearchFunctions1 (PotencialRedex(..), Found(..))

searchTermNeg :: Term -> (TermNF -> Found) -> Found
searchTermNeg (Var x) k = k (NegVarNF x)
searchTermNeg (Neg t) _ = PotRed (PrNeg t)
searchTermNeg (Conj t1 t2) _ = PotRed (PrConj t1 t2)
searchTermNeg (Disj t1 t2) _ = PotRed (PrDisj t1 t2)

searchTerm :: Term -> (TermNF -> Found) -> Found
searchTerm (Var x) k = k (PosVarNF x)
searchTerm (Neg t) k = searchTermNeg t k
searchTerm (Conj t1 t2) k =
    case searchTerm t1 k of
      Val v1 -> case searchTerm t2 k of
                  Val v2 -> k (ConjNF v1 v2)
                  PotRed pr -> PotRed pr
      PotRed pr -> PotRed pr
searchTerm (Disj t1 t2) k =
    case searchTerm t1 k of
      Val v1 -> case searchTerm t2 k of
                  Val v2 -> k (DisjNF v1 v2)
                  PotRed pr -> PotRed pr
      PotRed pr -> PotRed pr

search :: Term -> Found
search t = searchTerm t (\v -> Val v)
