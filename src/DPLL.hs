module DPLL where

import AST
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

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
  undefined

unitClause :: Expr -> Maybe (Char, Bool)
unitClause x = undefined

clauses :: Expr -> [Expr]
clauses e = undefined

allUnitClauses :: Expr -> [(Char, Bool)]
allUnitClauses e = undefined

unitPropagation :: Expr -> Expr
unitPropagation e = undefined
