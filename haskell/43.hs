{-
    1406357289, is a 0 to 9 pandigital number
    it also has a rather interesting sub-string divisibility property.
    Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17
    Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import Utils (perms,fromList,primes)

{-
    1. Generate 0-9 permutations
    2. Get each's substrings through divmod
    3. Get each's remainders from mod prime, filter all 0
 -}

split x k = (x `div` 10^k) `mod` 1000
splits x = map (split x) [6,5..0] 
detuple f (a, b) = f a b
pairmod = detuple mod

hasSubstringPrimeDivs x = all (==0) remains
    where
        z = zip (splits x) (take 7 primes)
        remains = map pairmod z

pandigs x = map fromList (perms x)
answer = sum $ filter hasSubstringPrimeDivs $ pandigs [9,8..0]


main = do
    print $ hasSubstringPrimeDivs 1406357289
    print $ answer




-- Cleaner but slow 
pairmod' xs = map modp [0..6]
    where 
        ps = take 7 primes
        modp j = (xs !! j) `mod` (ps !! j)
