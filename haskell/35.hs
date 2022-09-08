{-
    The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

    There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?
-}
import Utils (toList,fromList,isPrime,sameValues)


split' l xs = (take l xs, drop l xs)
split n xs
    | n == 0 = split' 0 xs
    -- | n >= l = split' l xs
    | l `mod` n == 0 = split' (l `div` n) xs 
    | otherwise = split' ((l `div` n) + 1) xs
    where l = length xs

rotate n xs = before ++ after
    where (after,before) = splitAt n xs

rots n = map fromList rotations
    where 
        xs = toList n
        rotations = map (\x -> rotate x xs) [1 ..length xs]

isCircular n = all isPrime $ rots n
circularPrimes p = filter isCircular $ 2 : [3, 5 ..p]
nCirculars p = length $ circularPrimes p

answer = length $ circularPrimes 1000000

main = do
    print $ rotate 1 [1,2,3]
    print $ nCirculars 100 == 13
    print $ rots 197 `sameValues` [197, 971, 719]
    print $ answer





-- Strat 1: Generate rotations, then check if they're a subset of the primes. 
    -- Extremely slow. 
    -- Why? Generates all primes, then does an expensive subset op for each rot
circularPrimes' p primes subset = filter (\x -> subset x ps) rs
                    where 
                        rs = map rots ps 
                        ps = takeWhile (<p) primes