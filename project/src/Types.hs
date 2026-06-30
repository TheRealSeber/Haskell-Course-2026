module Types
  ( Addr
  , showAddr
  , Value(..)
  , Op(..)
  , RangeOp(..)
  , Expr(..)
  , Content(..)
  , Sheet(..)
  ) where

import Data.Map (Map)

type Addr = (String, Int)

showAddr :: Addr -> String
showAddr (col, row) = col ++ show row

data Value
  = NumV  Double
  | BoolV Bool
  | StrV  String
  | ErrV  String   -- spreadsheet-level error, propagates like a value
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)

data RangeOp = SumR | AvgR
  deriving (Show, Eq)

data Expr
  = Ref    Addr
  | LitE   Value
  | BinOp  Op Expr Expr
  | RangeE RangeOp Addr Addr   -- e.g. SUM(A1:A3)
  deriving (Show, Eq)

data Content
  = Lit  Value
  | Form Expr
  deriving (Show, Eq)

newtype Sheet = Sheet { getSheet :: Map Addr Content }
  deriving (Show, Eq)
