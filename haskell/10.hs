{-
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.
-}

m = 2000000 - 1
terminator p xs = span (< p^2) xs
divisors p t = [x | x <- t, x `mod` p /= 0]
recurse ps p t = sieve ps (divisors p t)
sieve (p:ps) xs 
    | (h,t) <- terminator p xs = h ++ recurse ps p t

primes = 2 : sieve primes [3, 5..] 
primesUpTo n = takeWhile (<=n) primes
answer = sum $ primesUpTo m

main = do
    print $ sum (primesUpTo(10)) == 17
    print $ answer
    -- 10s with runhaskell, 0.1s with ghc

