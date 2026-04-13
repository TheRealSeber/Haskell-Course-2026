module Solution where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (permutations)
import Control.Monad (guard)
import Control.Monad.Writer (Writer, tell)

-- 1) Maze navigation
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze p d = Map.lookup p maze >>= Map.lookup d

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _    start []     = return start
followPath maze start (d:ds) = do
    next <- move maze start d
    followPath maze next ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start ds = go start ds [start]
  where
    go _ []     acc = return (reverse acc)
    go p (x:xs) acc = do
        next <- move maze p x
        go next xs (next : acc)

-- 2) Decoding a message
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- 3) Seating arrangements
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutations guests
    guard (all (not . isConflict) (neighbours perm))
    return perm
  where
    isConflict (a, b) = (a, b) `elem` conflicts || (b, a) `elem` conflicts

    neighbours []       = []
    neighbours [_]      = []
    neighbours xs@(x:_) = zip xs (drop 1 xs ++ [x])

-- 4) Result monad with warnings
data Result a = Failure String | Success a [String]
    deriving (Show)

instance Functor Result where
    fmap _ (Failure msg)  = Failure msg
    fmap f (Success x ws) = Success (f x) ws

instance Applicative Result where
    pure x = Success x []
    Failure msg   <*> _             = Failure msg
    _             <*> Failure msg   = Failure msg
    Success f ws1 <*> Success x ws2 = Success (f x) (ws1 ++ ws2)

instance Monad Result where
    return = pure
    Failure msg  >>= _ = Failure msg
    Success x ws >>= f = case f x of
        Failure msg   -> Failure msg
        Success y ws' -> Success y (ws ++ ws')

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge n
    | n < 0     = failure ("Negative age: " ++ show n)
    | n > 150   = do
        warn ("Suspiciously high age: " ++ show n)
        return n
    | otherwise = return n

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- 5) Evaluator with simplification log
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving (Eq, Show)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n)   = return (Lit n)
simplify (Add a b) = do
    a' <- simplify a
    b' <- simplify b
    simplifyStep (Add a' b')
simplify (Mul a b) = do
    a' <- simplify a
    b' <- simplify b
    simplifyStep (Mul a' b')
simplify (Neg e)   = do
    e' <- simplify e
    simplifyStep (Neg e')

simplifyStep :: Expr -> Writer [String] Expr
simplifyStep (Add (Lit 0) e) = do
    tell ["Add identity: 0 + e -> e"]
    return e
simplifyStep (Add e (Lit 0)) = do
    tell ["Add identity: e + 0 -> e"]
    return e
simplifyStep (Add (Lit a) (Lit b)) = do
    tell ["Const fold: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
    return (Lit (a + b))
simplifyStep (Mul (Lit 1) e) = do
    tell ["Mul identity: 1 * e -> e"]
    return e
simplifyStep (Mul e (Lit 1)) = do
    tell ["Mul identity: e * 1 -> e"]
    return e
simplifyStep (Mul (Lit 0) _) = do
    tell ["Zero absorption: 0 * _ -> 0"]
    return (Lit 0)
simplifyStep (Mul _ (Lit 0)) = do
    tell ["Zero absorption: _ * 0 -> 0"]
    return (Lit 0)
simplifyStep (Mul (Lit a) (Lit b)) = do
    tell ["Const fold: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
    return (Lit (a * b))
simplifyStep (Neg (Neg e)) = do
    tell ["Double negation: -(-e) -> e"]
    return e
simplifyStep e = return e

-- 6) ZipList
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- No lawful Monad instance for ZipList:
-- `pure` must be the neutral element of `<*>`, so `pure x = ZipList (repeat x)`
-- — infinite. Any positional `>>=` is forced to be the diagonal,
--     ZipList xs >>= f = ZipList [ getZipList (f (xs !! i)) !! i | i <- [0..] ].
-- Left identity `return a >>= f === f a` then fails whenever `f a` is finite:
-- the diagonal is infinite, `f a` is not. Associativity breaks for the same
-- reason — different branches may return lists of different lengths, so the
-- positional combination cannot agree with nested-list flattening.
