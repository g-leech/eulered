{-
    What is the 10 001st prime number?
-}

import Utils (primes)

-- zero-indexed
main = print $ primes !! (10001-1)