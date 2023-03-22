{-
    n^2 + an + b. Find the quadratic that produces the longest run of primes with
    |a| < 1000
    |b| <= 1000
-}

-- Strat 1: gridsearch and primes detection like an imperative pleb.
-- Strat 2: range restriction. rearrange the formula for a,b. 10x speedup.

import Utils (primes, maxOn, divisors)

{-
    Strat 2:
        Let n = 0. See that b is prime
        Let n = 1. Then (1 + a + b) is prime, and b is prime, so a is odd unless b=2.
-}
isPrime n 
    | n < 1 = False
    | otherwise = (divisors n) == [n,1]
nConsecutivePrimes (a, b) = length $ takeWhile isPrime formulae
                            where
                                formulae = [quad a b n | n <- [0..]]
                                quad a b n = n^2 + a*n + b

coefficients = [(a, b) | a <-as, b <-bs]
    where
        u = 999
        as = 2 : [-u,-u+2 .. u]
        bs = takeWhile (<=1000) primes

main = do
    -- print $ all (== True) $ map isPrime (map (quad 1 41) [0..39])
    -- print $ nConsecutivePrimes (1, 41)
    let (a, b) = maxOn nConsecutivePrimes coefficients
    let answer = a * b
    print $ answer
