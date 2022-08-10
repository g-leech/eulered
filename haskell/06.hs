{-
    The sum of the squares of the first ten natural numbers is 385
    The square of the sum of the first ten natural numbers is 3025
    Hence the difference between these is 2640.

    Find the difference between the sum of squares [1..100] and the square of the sum.
-}

sumSquares n = sum $ map (^2) [1..n]
squareSum n = sum [1..n] ^ 2
diffSqSum n = squareSum n - sumSquares n
answer = diffSqSum 100

main = do
    print $ diffSqSum 10 == 2640
    print $ answer 
