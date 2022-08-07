{-
    We can see that 28 is the first triangle number to have over five divisors.

    What is the value of the first triangle number to have over five hundred divisors?
-}

import Utils (factorPrimes)
import Data.List (group)

{-
    Strat
    Speedup via counting prime factor combis, over trial division
    Speedup via laziness and `head` 
-}

p = 500
tenTriangles = [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

triangulars = scanl1 (+) [1..]
-- one entry per divisor
groupedPrimeFactors n = group (factorPrimes n)
-- add 1 for `1`, then prod for # factor combinations
nDivisors n = product . map succ . map length $ groupedPrimeFactors n

ix = length [ x | x <- takeWhile (<=p) $ map nDivisors triangulars ] 
answer = triangulars !! ix

main = do
    print $ (take 10 triangulars) == tenTriangles
    print $ answer
    

{-
-- laughably slow
isFactor p n = n `mod` p == 0
trialDivision n = n : filter (`isFactor` n) [1..half]
                where half = n `div` 2
lfs n = length $ trialDivision n
head . takeWhile (<501) $ lfs triangulars
-}