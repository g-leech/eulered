module Utils where

-- import Data.List
import Data.Containers.ListUtils (nubOrd)
import Data.List (maximumBy)
import Data.Function (on)
import Data.List ((\\))


(¬) = not

between x y z = if (x <= y) then (y <= z) 
                else False

assert :: Bool -> String
assert False = error "assertion failed!"
assert _     = "ok"

dedupe xs = nubOrd xs
subset a b = null [x | x<-a, elem x b == False]

-- "explode"
-- toList :: Integer -> [Integer]
toList n 
    | n < 10    = [n]
    | otherwise = (recurse n) ++ [snip]
    where
        snip = n `mod` 10
        recurse n = toList $ n `div` 10

fromList n = foldl (\x y -> 10*x+y) 0 n


fac n = foldl1 (*) [2..n]

isFactor p n = n `mod` p == 0
factor n (p:ps)
    | n < p^2      = [n] 
    | p`isFactor`n = p : factor (n `div` p) (p:ps)
    | otherwise    = factor n ps
isOwnOnlyFactor n = (n == (factor n primes) !! 0)
primes = 2 : filter isOwnOnlyFactor [3,5..]
factorPrimes :: Int -> [Int]
factorPrimes n = factor n primes


fastprimes = 2 : filter ((==1) . length . primeFactors) [3,5..]
primeFactors n = factor n fastprimes
isPrime 1 = False
isPrime n = case (primeFactors n) of
                (_:_:_)   -> False
                _         -> True


sameValues x y = null (x \\ y) && null (y \\ x)



powerset [] = [[]]
powerset (x:xs) = [ x:ps | ps<-pow ] ++ pow
                    where pow = powerset xs
divisors n = dedupe . map product . 
                powerset $ factorPrimes n


replace old new char = map $ \char -> if char == old then new else char
remove e xs = filter (not . (`elem` e)) xs
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "bad n"


sort []     = []
sort (p:xs) = (sort lesser) ++ [p] ++ (sort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- Absurd.
p x = putStrLn $ x
-- listP xs = putStrLn $ intercalate " " (map show xs)

-- get index of s
look_up :: [String] -> String -> Maybe Int
look_up [] _ = Nothing
look_up (x:xs) s | not(x == s)  = fmap (1 +) (look_up xs s)
                 | otherwise    = Just 0


picks [] = []
picks (x:xs) = [(xs,x)] ++ rest
            where rest = [(x:ys,y) | (ys,y) <- picks xs]

perms [] = [[]]
perms xs = do
    (ys,x) <- picks xs
    zs     <- perms ys
    return (x:zs)

maxOn f = maximumBy (compare `on` f)