{-
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.
-}

import Utils (primes)

primesUpTo n = takeWhile (<n) primes

main = do
    -- print $ sum (primesUpTo(10)) == 17
    let answer = sum $ primesUpTo 2000000
    print $ answer
    -- 10s with runhaskell, 0.1s with ghc