module AST where

import Control.Applicative ((<|>))
-- import Data.Bits (Bits (xor))

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

data Expr
  = Var Char
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Const Bool
  deriving (Show, Eq)

freeVariable :: Expr -> Maybe Char
freeVariable (Const _) = Nothing
freeVariable (Var v) = Just v
freeVariable (Not e) = freeVariable e
freeVariable (Or x y) = freeVariable x <|> freeVariable y
freeVariable (And x y) = freeVariable x <|> freeVariable y

guessVariable :: Char -> Bool -> Expr -> Expr
guessVariable v b e = case e of
  Const b' -> Const b'
  Not e' -> Not $ guessVariable v b e'
  Or l r -> Or (guessVariable v b l) (guessVariable v b r)
  And l r -> And (guessVariable v b l) (guessVariable v b r)
  Var c ->
    if v == c
      then Const b
      else Var v

simplify :: Expr -> Expr
simplify (Const b) = (Const b)
simplify (Var v) = Var v
simplify (Not (Const b)) = Const (not b)
simplify (Not e) = Not e
simplify (Or x y) =
  let es = filter (/= Const False) [simplify x, simplify y]
   in if Const True `elem` es
        then Const True
        else case es of
          [] -> Const False
          [e] -> e
          [e1, e2] -> Or e1 e2
          _ -> error "Internal Error: Unreachable match met"
simplify (And x y) =
  let es = filter (/= Const True) [simplify x, simplify y]
   in if Const False `elem` es
        then Const False
        else case es of
          [] -> Const True
          [e] -> e
          [e1, e2] -> And e1 e2
          _ -> error "Internal Error: Unreachable match met"

unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Not Const"

satisfiable :: Expr -> Bool
satisfiable expr =
  case freeVariable expr of
    Nothing -> unConst expr
    Just v ->
      let trueGuess = simplify (guessVariable v True expr)
          falseGuess = simplify (guessVariable v False expr)
       in satisfiable trueGuess || satisfiable falseGuess

fixNegations :: Expr -> Expr
fixNegations expr =
  case expr of
    Not (Not x) -> fixNegations x
    Not (And x y) -> Or (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Or x y) -> And (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Const b) -> Const (not b)
    Not x -> Not (fixNegations x)
    And x y -> And (fixNegations x) (fixNegations y)
    Or x y -> Or (fixNegations x) (fixNegations y)
    x -> x

distribute :: Expr -> Expr
distribute expr =
  case expr of
    Or x (And y z) ->
      And
        (Or (distribute x) (distribute y))
        (Or (distribute x) (distribute z))
    Or (And y z) x ->
      And
        (Or (distribute x) (distribute y))
        (Or (distribute x) (distribute z))
    Or x y -> Or (distribute x) (distribute y)
    And x y -> And (distribute x) (distribute y)
    Not x -> Not (distribute x)
    x -> x

cnf :: Expr -> Expr
cnf expr =
  if updated == expr
    then expr
    else cnf updated
  where
    updated = distribute (fixNegations expr)

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
