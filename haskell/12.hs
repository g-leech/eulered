{-
    We can see that 28 is the first triangle number to have over five divisors.
    What is the value of the first triangle number to have over five hundred divisors?
-}

import Utils (factorPrimes,group)

{-
    Strat 2
    Speedup via counting prime factor combis
    Speedup via laziness
-}

triangulars = scanl1 (+) [1..]
-- one entry per divisor
groupedPrimeFactors n = group (factorPrimes n)
-- add 1 for the missing `1` factor, then prod for # factor combinations
nDivisors n = product $ map (succ . length) grps
    where grps = groupedPrimeFactors n

ix cap = length [ x | x <- takeWhile (<=cap) numDivs ] 
        where numDivs = map nDivisors triangulars

main = do
    let answer = triangulars !! (ix 500)
    print $ answer


{-
-- laughably slow
isFactor p n = n `mod` p == 0
trialDivision n = n : filter (`isFactor` n) [1..half]
                where half = n `div` 2
lfs n = length $ trialDivision n
head . takeWhile (<501) $ lfs triangulars
-}