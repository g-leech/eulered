{-
    Digit sums
    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 2^1000?
-}

toList :: Integer -> [Integer]
toList n 
    | n < 10    = [n]
    | otherwise = snip : recurse n
    where
        snip = n `mod` 10
        recurse n = toList $ n `div` 10

main = do 
    -- print $ (sum . toList $ 2^15) == 26
    let answer = sum . toList $ 2^1000
    print $ answer