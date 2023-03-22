{-
    F1 = 1 and F2 = 1.
    F12 is the first term to contain three digits.

    What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
-}

fibs = 1 : 1 : next 1 1
    where 
      next x y = z : (next y z) 
        where z = x + y

ixFirstFibNDigits n = length $ takeWhile (<n) lenFibs
    where
        digs 0 = []
        digs x = digs (x `div` 10) ++ [x `mod` 10]
        nDigits x = length $ digs x
        lenFibs = map nDigits fibs

main = do
    -- print $ nDigits (fibs !! 12-1) == 3
    let answer = 1 + ixFirstFibNDigits 1000
    print $ answer