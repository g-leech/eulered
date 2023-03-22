strToPyramid :: [Char] -> [[Int]]
strToIntList = map read . words
-- trying to get used to implicit parameters
strToPyramid = map strToIntList . lines

-- Bottom to top
-- Iterate along each row
backwards :: [Int] -> [Int] -> [Int]
backwards top bottom = currentSum : rest
    where
        (x:xs) = top
        (y:z:zs) = bottom
        currentSum = x + max y z
        rest = backwards xs (z:zs)
maxPathBottom tree = foldr1 backwards tree

main = do
    raw <- readFile "data/p067.txt" 
    let pyramid = strToPyramid raw
    let answer = head $ maxPathBottom pyramid
    print $ answer

