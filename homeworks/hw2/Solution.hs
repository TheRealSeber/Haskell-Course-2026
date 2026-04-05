module Solution where

-- Data type used throughout this homework
data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

-- 1) Functor for Sequence
instance Functor Sequence where
    fmap _ Empty        = Empty
    fmap f (Single x)   = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2) Foldable for Sequence (left-to-right traversal)
instance Foldable Sequence where
    foldMap _ Empty        = mempty
    foldMap f (Single x)   = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = length

-- 3) Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty

-- 4) Tail-recursive element search with explicit stack
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x s = go [s]
  where
    go []                   = False
    go (Empty      : rest)  = go rest
    go (Single y   : rest)  = y == x || go rest
    go (Append l r : rest)  = go (l : r : rest)

-- 5) Tail-recursive flatten to list
tailToList :: Sequence a -> [a]
tailToList s = go [s] []
  where
    go []                    acc = reverse acc
    go (Empty      : stack)  acc = go stack acc
    go (Single x   : stack)  acc = go stack (x : acc)
    go (Append l r : stack)  acc = go (l : r : stack) acc

-- 5) Tail-recursive Reverse Polish Notation evaluator
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go []               [result] = Just result
    go []               _        = Nothing
    go (TNum n : rest)  stack    = go rest (n : stack)
    go (TAdd   : rest)  (b:a:st) = go rest (a + b : st)
    go (TSub   : rest)  (b:a:st) = go rest (a - b : st)
    go (TMul   : rest)  (b:a:st) = go rest (a * b : st)
    go (TDiv   : rest)  (b:a:st)
        | b == 0                  = Nothing
        | otherwise               = go rest (a `div` b : st)
    go _                _        = Nothing

-- 6) Functions expressed via foldr / foldl

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- 7) Run-length encoding via folds

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr f []
  where
    f x []              = [(x, 1)]
    f x ((y, n) : rest)
        | x == y        = (y, n + 1) : rest
        | otherwise     = (x, 1) : (y, n) : rest

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []
