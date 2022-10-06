{-
    First prime in the first eight-prime family 

    By replacing the 1st digit of the 2-digit number *3, it turns out that 6/9 are prime: 
    13, 23, 43, 53, 73, and 83, are all prime.

    replacing 56**3 with the same digit * is the first 5-dig example with 7/10: 
    56003, 56113, 56333, 56443, 56663, 56773, and 56993. 
    Consequently 56003, being the first member of this family, is the smallest prime with this property.

    Find the smallest prime which, by replacing part of the number (not necessarily adjacent) 
    with the same digit, is part of an eight prime value family.
-}
import Utils (isPrime,fromList,replaceAt,toList,fpow)
import Data.List (findIndices,nub)

-- Note 1: only need 3 wildcards
-- Note 2: init search at 56003
-- Note 3: spurious solutions like 0011 don't count
-- Strat 1: numbers as digit lists, wildcard -1s. Must be a simpler way but I hate stringifying.


--------------------------------------
-- Take family of numbers (as single flat wildcarded list), count primes
getNulls = findIndices (==(-1))
replaceAll _ _ [] = []
replaceAll old new (x:xs) = if old == x then new : rec
                            else x : rec
                            where rec = replaceAll old new xs 
family xs = filter (>=10) $ map fromList members
    where 
        members = map plugin [0..9]
        plugin j = replaceAll (-1) j xs

countPrimes xs = length $ filter isPrime xs
countFamPrimes = countPrimes . family 
--------------------------------------
-- Gen all families
wildcards a = concatMap exhaustiveWilds possiblePrimes
    where possiblePrimes = filter notDiv5 [a,a+2..]

exhaustiveWilds x = threewalk $ toList x
notDiv5 x = x `mod` 5 /= 0

sentinelise i xs = replaceAt i (-1) xs
walk [x] = []
walk xs = map (\i -> sentinelise i xs) [0..n]
    where n = length xs-2
walk' = concat . map walk
-- Note 1: only generating <= 3 wildcards per number
numWilds = 3
-- apply walk 3 times with fpow 3
threewalk xs = nub $ fpow numWilds walk' [xs]

lenIth xs i = length $ toList (xs !! i)
findFamilyWithNPrimes wilds n = (head . head) 
                                [ g | xs<-wilds, 
                                    let g = filter isPrime $ family xs,
                                    length g == n,
                                    -- Note 3
                                    lenIth g 0 == lenIth g 1
                                ]
-- Note 2
-- Parametrising this properly --> 50% speed decrease!
answer = findFamilyWithNPrimes (wildcards 56003) 8

main = do
    -- print $ findFamilyWithNPrimes (wildcards 3) 6 == 13
    -- print $ findFamilyWithNPrimes (wildcards 3) 7 == 56003
    print $ answer