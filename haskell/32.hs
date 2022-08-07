{-
    d with length n is pandigital if it has all digits 1 to n exactly once; 
    d = 15234 is 1 through 5 pandigital.

    The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
    containing multiplicand, multiplier, and product is 1 through 9 pandigital.

    Find the sum of all products whose multiplicand/multiplier/product identity can be written as 
    a 1 through 9 pandigital.

    Some products have dupes
-}
import Utils (toList,fromList,sort,dedupe)

-- Strat 1: Bound it and check all nxm

{- 
    bound: 987654321 is the largest pandigital. We can do better.
    
    What magnitudes give us 9 digits total?
        (length 2) x (length 3); (ab)*(cde) = efgh
            Lowest 12 * 345 gives 4 digits, 98 * 765 gives 5
        length 1 x length 4;
            5 * 9876
    Either way, product has to be 4 digits (9-5). So ab < 10000 

    I got bored so: amax = 9876     bmax = 98
-}

-- hasOnly1ToNOnce
isPandigital d n
    | between = take n (sort ds) == [1..n]   
    | otherwise = False
    where
        between = if (n <= l) then (l <= 9) else False
        ds = toList d
        l = length $ ds

conditions a b = a /= b && 
                 a*b < 10000 && 
                 isPandigital conc 9
                 where conc = fromList [a,b,(a*b)]
rawPans = [ a*b | a<-[1..9876], b<-[1..98], conditions a b]

answer = sum $ dedupe rawPans


main = do
    print $ isPandigital 15234 5
    print $ isPandigital 165234 5 
    print $ isPandigital (fromList [39, 186, 7254]) 9
    print $ answer