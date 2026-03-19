module Solution where

-- 1) Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 || odd n = []
  | otherwise =
      [ (p, q)
      | p <- [2 .. n `div` 2]
      , let q = n - p
      , isPrime p
      , isPrime q
      ]

-- 2) Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
  | x <- xs
  , y <- xs
  , x < y
  , gcd x y == 1
  ]

-- 3) Sieve of Eratosthenes and bounded primality
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n
  | n < 2 = []
  | otherwise = sieve [2 .. n]

isPrimeViaPrimesTo :: Int -> Bool
isPrimeViaPrimesTo n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

-- 4) Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
  | null a || null b = []
  | length (head a) /= length b = error "Incompatible matrix dimensions"
  | otherwise =
      [ [ sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]]
        | j <- [0 .. n - 1]
        ]
      | i <- [0 .. m - 1]
      ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- 5) k-element permutations (ordered, without repetition)
permutations :: Int -> [a] -> [[a]]
permutations k _ | k < 0 = []
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
  | (y, rest) <- select xs
  , ys <- permutations (k - 1) rest
  ]

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

-- 6) Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xt) ys@(y:yt)
  | x < y = x : merge xt ys
  | x > y = y : merge xs yt
  | otherwise = x : merge xt yt

hamming :: [Integer]
hamming = 1 : merge (map (2 *) hamming) (merge (map (3 *) hamming) (map (5 *) hamming))

-- 7) Integer power with strict accumulator
power :: Int -> Int -> Int
power _ e | e < 0 = error "Negative exponents are not supported"
power b e = go 1 e
  where
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)

-- 8) Running maximum: seq vs bang patterns
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "listMaxSeq: empty list"
listMaxSeq (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) =
      let acc' = max acc y
       in acc' `seq` go acc' ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "listMaxBang: empty list"
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y:ys) = go (max acc y) ys

-- 9) Infinite prime stream and unbounded primality check
primes :: [Int]
primes = sieve [2 ..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

-- 10) Mean: lazy and strict accumulations, plus one-pass variance
meanLazy :: [Double] -> Double
meanLazy [] = error "meanLazy: empty list"
meanLazy xs = total / fromIntegral count
  where
    (total, count) = go xs (0, 0)
    go [] (s, n) = (s, n)
    go (y:ys) (s, n) = go ys (s + y, n + 1)

mean :: [Double] -> Double
mean = meanStrict

meanStrict :: [Double] -> Double
meanStrict [] = error "meanStrict: empty list"
meanStrict xs = total / fromIntegral count
  where
    (total, count) = go xs 0 0
    go [] !s !n = (s, n)
    go (y:ys) !s !n = go ys (s + y) (n + 1)

meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "meanVariance: empty list"
meanVariance xs = (mu, variance)
  where
    (sumX, sumX2, count) = go xs 0 0 0
    mu = sumX / fromIntegral count
    variance = sumX2 / fromIntegral count - mu * mu

    go [] !s !s2 !n = (s, s2, n)
    go (y:ys) !s !s2 !n = go ys (s + y) (s2 + y * y) (n + 1)