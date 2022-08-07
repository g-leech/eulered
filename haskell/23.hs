{-
    perfect number: the sum of proper divisors of n === n. 
    For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
    which means that 28 is a perfect number.

    n is deficient if the sum of its proper divisors < n 
    n is abundant if this sum > n.

    12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 
    so the smallest number that can be written as the sum of two abundant numbers is 24. 
    it can be shown that all integers greater than 28123 can be written as two abundant numbers. 

    this upper limit cannot be reduced any further by analysis 
    though we know the greatest noncomposite-abundant number is lower.

    Find the sum of all the positive integers which are not binary-composite-abundant
-}
import Utils (divisors,dedupe,(¬))

n = 28123

propaDivisors n = filter (<n) $ divisors n
divSum n = sum $ propaDivisors n
isAbundant n = n < divSum n
abundants = filter isAbundant [1..]

-- Strat 1: Seeking a + b = abundant; so try isAbundant(b - abundant)
-- What can't be written as the sum of two abundant numbers
isNotSum2Abundants n = (¬) $ any diffIsAbundant abunsHalf
                    where
                        diffIsAbundant k = isAbundant (n-k)
                        abunsHalf = takeWhile (<= n `div` 2) abundants

answer = sum $ filter (isNotSum2Abundants) [1..n]

main = do
    print $ isAbundant 12 == True
    print $ (dedupe $ map isAbundant [1..11]) == [False]
    print $ not (isNotSum2Abundants 24) 
    print $ answer

    -- isPerfect n = n == divSum n
    -- print $ isPerfect 28 == True