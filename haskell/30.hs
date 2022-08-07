{-
    only three numbers can be written as the sum of fourth powers of their digits:

    1634 = 14 + 64 + 34 + 44    8208 = 84 + 24 + 04 + 84    9474 = 94 + 44 + 74 + 44
    As 1 = 14 is not a sum it is not included.
    The sum of these numbers is 1634 + 8208 + 9474 = 19316.

    sum all the numbers that can be written as the sum of fifth powers of their digits.
-}
import Utils (toList)

{- 
    need a cap `c = d x 9^n`. 
    How many digits can we expect c to have?
    (d in [3..6] gives c with 6 digits.)
    Let 10^d-1 <= c <= 10^d
    Log laws tell us that d is less than 6.48, so d = 6
-}
cap = 6 * 9^5

empowerDigits p n = map (^p) (toList n)
isSumNthPowerOfDigits n p = n == sum (empowerDigits p n)
isLegit x = isSumNthPowerOfDigits x 5

answer = sum $ filter isLegit [2..cap]

main = do
    print $ isSumNthPowerOfDigits 1634 4 
    print $ answer