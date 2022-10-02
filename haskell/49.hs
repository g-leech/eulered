{-
    The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330
    (i) each of the three terms are prime, and
    (ii) each of the 4-digit numbers are permutations of one another.

    There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
    exhibiting this property, but there is one other 4-digit increasing sequence.

    What 12-digit number do you form by concatenating the three terms in this sequence?
-}

-- hidden lines: 19
import Utils (toList,isPrime,dedupe,sort)

-- Strat 1: Naive n^3 loop: x < y < z, z-y == y-x. General case.
-- Strat 2: Special case with 3330s.

a = 3330
primes4 = filter isPrime [1000..9999]

isPerm m n = slist m == slist n 
    where slist = sort . toList 
concatNums n j [x] = j + x
concatNums n j (x:xs) = concatNums n acc xs
    where acc = (j + x)*10^n
verySpecific3330 = drop 1 [ [x, y, z] | x<-primes4,
                            let y = x + a,
                            let z = y + a,
                            isPrime z, isPrime y,
                            isPerm x y, isPerm x z
                          ]
answer = concatNums 4 0 $ concat verySpecific3330


main = do
    print $ answer 
