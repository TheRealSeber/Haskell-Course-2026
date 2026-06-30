module Evaluator
  ( evalSheet
  , evalExpr
  ) where

import Types
import Graph    (buildGraph, topoSortFull, expandRange)
import Data.Map (Map)
import qualified Data.Map as Map
type Env = Map Addr Value

evalSheet :: Sheet -> Map Addr Value
evalSheet sheet@(Sheet cells) =
  let graph                = buildGraph sheet
      (cyclicAddrs, order) = topoSortFull graph
      seedEnv = Map.fromList [(a, ErrV "cycle") | a <- cyclicAddrs]
  in foldl' (evalCell cells) seedEnv order

evalCell :: Map Addr Content -> Env -> Addr -> Env
evalCell cells env addr =
  case Map.lookup addr cells of
    Nothing       -> env
    Just (Lit v)  -> Map.insert addr v env
    Just (Form e) ->
      let val = case evalExpr env e of
                  Right v  -> v
                  Left err -> ErrV err
      in Map.insert addr val env

evalExpr :: Env -> Expr -> Either String Value
evalExpr _   (LitE v)           = Right v
evalExpr env (Ref addr)         =
  case Map.lookup addr env of
    Nothing       -> Left ("undefined: " ++ showAddr addr)
    Just (ErrV e) -> Left e
    Just v        -> Right v
evalExpr env (BinOp op l r)     = do
  lv <- evalExpr env l
  rv <- evalExpr env r
  applyOp op lv rv
evalExpr env (RangeE rop a1 a2) = do
  vals <- mapM (\a -> evalExpr env (Ref a)) (expandRange a1 a2)
  applyRangeOp rop vals

applyOp :: Op -> Value -> Value -> Either String Value
applyOp Add (NumV a) (NumV b) = Right (NumV (a + b))
applyOp Sub (NumV a) (NumV b) = Right (NumV (a - b))
applyOp Mul (NumV a) (NumV b) = Right (NumV (a * b))
applyOp Div _        (NumV 0) = Left "division by zero"
applyOp Div (NumV a) (NumV b) = Right (NumV (a / b))
applyOp op  _        _        = Left ("type error in " ++ show op)

applyRangeOp :: RangeOp -> [Value] -> Either String Value
applyRangeOp SumR vals = do
  nums <- mapM toNum vals
  return (NumV (sum nums))
applyRangeOp AvgR vals = do
  nums <- mapM toNum vals
  if null nums
    then Left "AVG of empty range"
    else return (NumV (sum nums / fromIntegral (length nums)))

toNum :: Value -> Either String Double
toNum (NumV n) = Right n
toNum (ErrV e) = Left e
toNum v        = Left ("expected number, got: " ++ show v)
