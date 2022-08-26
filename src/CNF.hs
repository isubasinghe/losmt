module CNF where

import AST

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
