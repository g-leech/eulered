{-
    The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
    Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
    (Please note that the palindromic number, in either base, may not include leading zeros.)
-}
import Utils (toList)


toBin 0 = []
toBin n = toBin (n `div` 2) ++ [n `mod` 2] 

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) 
    | x == last xs  = isPalindrome (init xs) 
    | otherwise     = False
palin10n2 n = isPalindrome (toList n) && isPalindrome (toBin n)

answer = sum [x | x<-[1..1000000], 
                palin10n2 x]


main = do
    print $ palin10n2 585 
    print $ answer
