{-
    The number 3797 has an interesting property. Being prime, it is possible to continuously remove digits from left to right, 
    and remain prime at each stage: 3797, 797, 97, and 7. 
    Similarly we can work from right to left: 3797, 379, 37, and 3.

    Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

    NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
-}
import Utils (isPrime,primes)

-- Strat 1: Break functional kayfabe and cast to and from list
-- Strat 2: Mod log10

floor' = 7
cap = 11

-- truncateR 
init' x = x `div` 10
-- truncateL
tail' x = x `mod` 10^y
    where 
        y = floor (log fx / log 10) 
        fx = fromIntegral x

trunc n f
    | n < 10     = True
    | otherwise  = isPrime (f n) && trunc (f n) f

truncatable n = (trunc n init' && 
                trunc n tail') && 
                n > floor'


answer = sum $ take cap $ filter truncatable primes

main = do
    print $ truncatable 3797
    print $ truncatable 5
    print $ answer



-- Grody truncateL
-- tail' :: Int -> Int
-- tail' x = fromList (tail (toList x))