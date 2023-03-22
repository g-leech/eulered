{-
    First prime in the first eight-prime family 
    
    By replacing the 1st digit of the 2-digit number *3, it turns out that 6/9 are prime: 13, 23, 43, 53, 73, and 83, are all prime.
    replacing 56**3 with the same digit * is the first 5-dig example with 7/10: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. 
    Find the smallest prime which, by replacing part of the number (not necessarily adjacent) 
    with the same digit, is part of an eight prime value family.
-}

-- hidden lines: 22
import Utils (isPrime,fromList,replaceAt,toList,fpow,dedupe)
import Data.List (findIndices)


-- Note 1: only need 3 wildcards
-- Note 2: init search at 56003 from the prompt
-- Note 3: spurious solutions like 0011 don't count
-- Strat 1: numbers as digit lists, wildcard -1s. Must be a simpler way but I hate stringifying.

-- Take family of numbers (as single flat wildcarded list), count primes
replaceAll _ _ [] = []
replaceAll old new (x:xs) = if old == x then new : rec
                            else x : rec
                            where rec = replaceAll old new xs 
family xs = filter (>=10) $ map fromList members
    where 
        members = map plugin [0..9]
        plugin j = replaceAll (-1) j xs

walk [x] = []
walk xs = map (\i -> sentinelise i xs) [0..n]
            where 
                n = length xs-2
                sentinelise i xs = replaceAt i (-1) xs

-- Note 1: only generating <= 3 wildcards per number
-- so apply walk 3 times with fpow 3
threewalk xs = dedupe $ fpow numWilds walk' [xs]
    where 
        numWilds = 3
        walk' = concat . map walk

-- Gen all families
wildcards a = concatMap exhaustiveWilds possiblePrimes
    where 
        exhaustiveWilds x = threewalk $ toList x
        possiblePrimes = filter notDiv5 [a,a+2..]
        notDiv5 x = x `mod` 5 /= 0

famsWithNPrimes ws n = [ g | xs<-ws, 
                            let g = filter isPrime $ family xs,
                            length g == n,
                            sameLength g -- Note 3
                        ]
                        where
                            sameLength xs = lenIth xs 0 == lenIth xs 1
                            lenIth xs i = length $ toList (xs !! i)
-- Note 2
answer = firstNPrime (wildcards 56003) 8
    where firstNPrime wilds n = (head . head) (famsWithNPrimes wilds n)

main = do
    -- print $ firstNPrime (wildcards 3) 6 == 13
    -- print $ firstNPrime (wildcards 3) 7 == 56003
    print $ answer