{-
    The largest palindrome from the product of two 2-digit numbers is 9009 = 91 × 99.
    Find the largest palindrome made from the product of two 3-digit numbers.
-}
-- Strat 1: do the declasse thing and stringify 
    -- isPalindrome n = reverse (show n) == show n

-- Strat 2: Classy recursion eating into the centre

import Utils (toList)


hi p = 10^p - 1
decrange p = [u, (u-1) ..1]
    where u = hi p

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) 
    | x == last xs  = isPalindrome (init xs) 
    | otherwise     = False

-- This would be idiotic but for the laziness of `head`:
palindromes xs = [x*y | x<-xs, y<-xs, 
                    isPalindrome (toList (x*y))] 
maxPalindrome p = head (palindromes (decrange p))


main = do
    print $ (maxPalindrome 2) == 9009
    let answer = maxPalindrome 3
    print $ answer
