{-
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
-}

{-
    Terminator: if n is composite, then ∃ a prime factor p <= sqrt(n)
    https://proofwiki.org/wiki/Composite_Number_has_Prime_Factor_not_Greater_Than_its_Square_Root
    By modus tollens, if ~∃ p <= sqrt(n), n is prime
    Therefore, only test primes up to sqrt(n). 
    Therefore, only test p^2 up to n. 

    Strat: circular reasoning
    factors are leaves of the division tree (1)
    being the only factor of yourself via (1) and the `primes` (2 <- 1 & 3)
    the primes are 2 and the odd numbers which are their own only factors (3 <- 2)
-}
isFactor p n = n `mod` p == 0
factor n (p:ps)
    | n < p^2      = [n] 
    | p`isFactor`n = p : factor (n `div` p) (p:ps)
    | otherwise    = factor n ps

isOwnOnlyFactor n = (n == (factor n primes) !! 0)
primes = 2 : filter isOwnOnlyFactor [3,5..]
primeFaxxors n = factor n primes


main = do
    -- print $ last (primeFaxxors 13195) == 29
    let answer = last (primeFaxxors 600851475143)
    print $ answer
