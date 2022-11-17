{-
    The first two consecutive numbers to have two distinct prime factors are:
    14 = 2 × 7    15 = 3 × 5

    The first three consecutive numbers to have three distinct prime factors are:
    644 = 2² × 7 × 23    645 = 3 × 5 × 43    646 = 2 × 17 × 19.

    Find the first four consecutive integers to have four distinct prime factors each. 
    What is the first of these numbers?
-}

-- Strat 1: Generate all sublists (tails) of factor lengths 
--          and search each sublist for the target.
--          Terminate on hit. Lovely and terse:
-- subIndex = findIndex (isPrefixOf subl) (tails s)

-- # hidden lines: 9
import Utils (subIndex,dedupe,factorPrimes)


numPFactors n = (length . dedupe) (factorPrimes n)
nList n i = concat $ replicate n [i]
nConsecNPrimes n = (indexer nums) + 1
    where
        indexer = subIndex (nList n n)
        nums = map numPFactors [1..]


answer = nConsecNPrimes 4 


main = do
    print $ nConsecNPrimes 2 == 14
    print $ nConsecNPrimes 3 == 644
    print $ answer