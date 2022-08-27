module SAT.DPLL where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import SAT.AST hiding (satisfiable)
import SAT.CNF (cnf)

literals :: Expr -> Set Char
literals (Var v) = S.singleton v
literals (Not e) = literals e
literals (And x y) = S.union (literals x) (literals y)
literals (Or x y) = S.union (literals x) (literals y)
literals (Const _) = S.empty

data Polarity = Positive | Negative | Mixed
  deriving (Show, Eq)

literalPolarity :: Expr -> Char -> Maybe Polarity
literalPolarity (Var v) v'
  | v == v' = Just Positive
  | otherwise = Nothing
literalPolarity (Not (Var v)) v'
  | v == v' = Just Negative
  | otherwise = Nothing
literalPolarity e v =
  case e of
    And x y -> combinePolarities [x, y]
    Or x y -> combinePolarities [x, y]
    Not x -> error $ "Not in CNF: negation of a non-literal" ++ show x
    Const _ -> Nothing
  where
    combinePolarities es =
      let polarities = mapMaybe (flip literalPolarity v) es
       in case polarities of
            [] -> Nothing
            ps ->
              if all (== Positive) ps
                then Just Positive
                else
                  if all (== Negative) ps
                    then Just Negative
                    else Just Mixed

literalElimination :: Expr -> Expr
literalElimination e =
  let ls = S.toList (literals e)
      ps = map (literalPolarity e) ls

      -- Find assignments we can make
      extractPolarized :: Char -> Maybe Polarity -> Maybe (Char, Bool)
      extractPolarized v (Just Positive) = Just (v, True)
      extractPolarized v (Just Negative) = Just (v, False)
      extractPolarized _ _ = Nothing

      -- Find *all* possible assignments
      assignments :: [(Char, Bool)]
      assignments = catMaybes $ zipWith extractPolarized ls ps

      -- Apply all the assignments.
      replacers :: [Expr -> Expr]
      replacers = map (uncurry guessVariable) assignments

      replaceAll :: Expr -> Expr
      replaceAll = foldl (.) id replacers
   in replaceAll e

unitClause :: Expr -> Maybe (Char, Bool)
unitClause (Var v) = Just (v, True)
unitClause (Not (Var v)) = Just (v, False)
unitClause _ = Nothing

clauses :: Expr -> [Expr]
clauses (And x y) = clauses x ++ clauses y
clauses expr = [expr]

allUnitClauses :: Expr -> [(Char, Bool)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagation :: Expr -> Expr
unitPropagation expr = replaceAll expr
  where
    assignments :: [(Char, Bool)]
    assignments = allUnitClauses expr

    replaceAll :: Expr -> Expr
    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)

satisfiable :: Expr -> Bool
satisfiable expr =
  case freeVariable expr' of
    Nothing -> unConst $ simplify expr'
    Just v ->
      let trueGuess = simplify (guessVariable v True expr)
          falseGuess = simplify (guessVariable v False expr)
       in satisfiable trueGuess || satisfiable falseGuess
  where
    -- Apply our backtracking search *after* literal elimination
    -- and unit propagation have been applied!
    expr' = literalElimination $ cnf $ unitPropagation expr
