{-
    F1 = 1 and F2 = 1.
    F12, is the first term to contain three digits.

    What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
-}

import Data.List (elemIndex)

n = 1000

next x y = z : (next y z) 
           where z = x + y
fibs = 1 : 1 : next 1 1

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
nDigits x = length $ digs x
lenFibs = map nDigits fibs
ixFirstFibToHaveNDigits n = length $ takeWhile (<n) lenFibs

answer = 1 + ixFirstFibToHaveNDigits n

main = do
    print $ nDigits (fibs !! 11) == 3
    print $ answer