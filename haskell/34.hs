{-
    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

    Find the sum of all numbers which are equal to the sum of the factorial of their digits.

    Note: As 1! = 1 and 2! = 2 are not sums they are not included.
-}
import Utils (toList)

{-
    Not given a cap here so find it ourselves:
    "abc" = fac a + fac b + fac c 
    for digit length `l`, 
        max fac sum is l x 9! 
        max tens is 10^l - 1
    10^l - 1 = l * 362880
    l = log10 l + 5.6
-}
l = 7 -- The funcs cross ~ l = 6.4.

-- Extremely slow
fac n = product [1..n]
isFacSum n = n == (sum $ map fac (toList n))

answer = sum $ filter isFacSum [10..10^l]

main = do
    print $ isFacSum 145
    print $ isFacSum 2
    print $ answer