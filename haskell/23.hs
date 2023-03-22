{-
    perfect number: the sum of proper divisors of n === n. 
    For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
    which means that 28 is a perfect number.

    n is deficient if the sum of its proper divisors < n 
    n is abundant if this sum > n.

    12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 
    so 24 is the smallest sum of two abundant numbers
    it can be shown that all integers greater than 28123 can be written as two abundant numbers. 

    this upper limit cannot be reduced any further by analysis 
    though we know the greatest noncomposite-abundant number is lower.

    Find the sum of all the positive integers which are not binary-composite-abundant
-}
import Utils (divisors,(¬))


isAbundant n = n < divSum n
    where
        propaDivisors n = filter (<n) $ divisors n
        divSum n = sum $ propaDivisors n

-- Strat 1: Seeking a + b = abundant; so try isAbundant(b - abundant)
-- What can't be written as the sum of two abundant numbers
isNotSum2Abundants n = (¬) (any diffIsAbundant abunsHalf)
                    where
                        diffIsAbundant k = isAbundant (n-k)
                        abundants = filter isAbundant [1..]
                        abunsHalf = takeWhile (<= n `div` 2) abundants

main = do
    -- print $ isAbundant 12 == True
    let answer = sum $ filter (isNotSum2Abundants) [1..28123]
    print $ answer

