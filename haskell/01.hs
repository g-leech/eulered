{- 
    If we list all the natural numbers below 10 that are multiples of 3 or 5, 
    we get 3, 5, 6 and 9. The sum of these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.
-}

multp x p = x `mod` p == 0
isMult3or5 x = multp x 3 || multp x 5

main = do
    -- print $ sum $ filter isMult3or5 [1..9] == 23
    let answer = sum $ filter isMult3or5 [1..999]
    print $ answer

