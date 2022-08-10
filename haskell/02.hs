{-
        Sum Even Fibonacci numbers
	By starting with 1 and 2, the first 10 terms will be:
		1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

	By considering the terms <= four million, find the sum of the even-valued terms.
-}

-- recursive addsucc
next x y = z : (next y z) 
           where z = x + y
-- init to Fibonacci and clip leading `1`
fibs = tail (1 : 1 : next 1 1)
underCap x = (x <= 4000000)
answer = sum . filter even . takeWhile underCap $ fibs

main = do
    print $ answer






{-
-- naive approach: incredibly slow
fibSlo n = case n of
        1 -> 1
        2 -> 2
        n -> fibSlo (n-1) + fibSlo (n-2)


-- combinator approach
-- shift left and add list to itself
addShiftedList xs = zipWith (+) xs (tail xs)
fib n = lookup !! n where
        lookup = 1 : 1 : addShiftedList lookup
infFibs = map fib [1..]

-}
