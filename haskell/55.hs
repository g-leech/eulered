{-
    Lychrel numbers
    A number that never forms a palindrome through reverse-add is a Lychrel number. 
    assume that a number is Lychrel until proven otherwise. 

    47 is nonLychrel, degree 1
    349 is nonLychrel, degree 3
    196 is Lychrel 
    4994 is Lychrel and palindromic!

    also assume every number <10000 either 
    (i) becomes a palindrome in <50 iterations
    (ii) is effectively Lychrel

    How many Lychrel numbers are there below ten-thousand?
-}
import Utils (toList,fpow)

cap = 10000

digReverse i = read . reverse $ show i
iteration i = i + digReverse i

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
    | x == last xs  = isPalindrome (init xs) 
    | otherwise     = False

isUnderNLychrel :: Int -> Integer -> Bool
isUnderNLychrel n x
    | n == 0 = True
    | pal = False
    | otherwise = isUnderNLychrel (n-1) ix
    where 
        pal = isPalindrome $ toList ix
        ix = iteration x

isLychrel = isUnderNLychrel 50
answer = length lychrels
    where
        lychrels = filter (==True) bools
        bools = map isLychrel [1..cap-1]

main = do
    print $ map isLychrel [47,196,4994] == [False, True, True]
    print $ answer