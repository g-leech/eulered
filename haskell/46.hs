{-
    Goldbach's other conjecture
    "every odd composite number can be written as the sum of a prime and twice a square"

    9 = 7 + 2×1^2
    15 = 7 + 2×2^2
    21 = 3 + 2×3^2
    the conjecture was false.

    What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}

import Utils (primes,isPrime,isInt,any',(¬))

-- Strat 1: LINEARRRR

isTwiceSquare x = isInt $ sqrt(fx / 2)
    where fx = fromIntegral x
goldbach x = any' [True | p<-ps, isTwiceSquare (x - p)]
    where ps = takeWhile (<x) primes

oddComposites = filter ((¬).isPrime) [33,35..]
answer = head $ filter ((¬).goldbach) oddComposites


main = do
    print $ goldbach 9
    print $ answer