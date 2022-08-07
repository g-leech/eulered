{-
    The largest palindrome from the product of two 2-digit numbers is 9009 = 91 × 99.
    Find the largest palindrome made from the product of two 3-digit numbers.
-}
-- Strat 1: do the declasse thing and stringify 

hi p = 10^p - 1
decrange p = [hi p, (hi p)-1 .. 1]
isPalindrome n = reverse (show n) == show n

-- This would be idiotic but for the laziness of `head`:
palindromes xs = [x*y | x<-xs, y<-xs, isPalindrome (x*y)] 
maxPalindrome p = head (palindromes (decrange p))
answer = maxPalindrome 3

main = do
    print $ (maxPalindrome 2) == 9009
    print $ answer
