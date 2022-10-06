module Utils where

-- import Data.List
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List ((\\),maximumBy,elemIndex,findIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as Set


(¬) = not
all' xs = all (==True) xs
between x y z = if (x <= y) then (y <= z) 
                else False
any' :: [Bool] -> Bool
any' = any (==True)


assert :: Bool -> String
assert False = error "assertion failed!"
assert _     = "ok"


dedupe xs = nubOrd xs
subset a b = null [x | x<-a, elem x b == False]
find x xs = fromJust (elemIndex x xs)
--  looks for the first occurrence of a sublist `sub` in the list, returns index of 1st element of `sub`
subIndex sub str = fromJust $ rawSubIndex sub str
rawSubIndex sub str = findIndex (prefixes sub) (tails str)
prefixes pre xs = all' $ map (\x -> fst x == snd x) $ zip pre xs
tails [] = []
tails xs = [xs] ++ tails (drop 1 xs)


-- "explode"
-- toList :: Integer -> [Integer]
toList n 
    | n < 10    = [n]
    | otherwise = (recurse n) ++ [snip]
    where
        snip = n `mod` 10
        recurse n = toList $ n `div` 10

-- only works with single-digit elements
fromList xs = foldl (\x y -> 10*x+y) 0 xs
isInt x = x == fromInteger (round x)


toBin 0 = []
toBin n = toBin (n `div` 2) ++ [n `mod` 2] 

toStr sep xs = foldr (\a b-> glue a b) "" xs
            where 
                glue a b = a ++ sepit b
                sepit b = (if b=="" then b 
                            else sep ++ b)

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
-- Shots fired
isPrime 1 = False
isPrime n = case (factorPrimes n) of
                (_:_:_)   -> False
                _         -> True
-- fastprimes = 2 : filter hasOneFactor [3,5..]
            -- where hasOneFactor = ((==1) . length . factorPrimes)


powerset [] = [[]]
powerset (x:xs) = [ x:ps | ps<-pow ] ++ pow
                    where pow = powerset xs
divisors n = dedupe . map product . 
                powerset $ factorPrimes n


-- terminator p xs = span (< p^2) xs
-- divisors p t = [x | x <- t, x `mod` p /= 0]
-- recurse ps p t = sieve ps (divisors p t)
-- sieve (p:ps) xs 
--     | (h,t) <- terminator p xs  =   h ++ recurse ps p t

-- primes = 2 : sieve primes [3, 5..] 


sameValues x y = null (x \\ y) && null (y \\ x)

allSame [x] = True
allSame (x:xs) 
    | x == head xs = allSame xs
    | otherwise = False


zip' :: [a] -> [b] -> [(a,b)]
zip' (a:as) (b:bs) = (a,b) : zip as bs
zip' _      _      = []

mapreduce f xs = foldl1 (*) $ map f xs


-- Numeric tail!
tail' x = x `mod` 10^y
    where 
        y = floor (log fx / log 10) 
        fx = fromIntegral x

-- Numeric concat (for fixed-width elements)
concatNums n j [x] = j + x
concatNums n j (x:xs) = concatNums n ((j + x)*10^n) xs


replaceAt n new (x:xs)
   | n == 0     = new:xs
   | otherwise  = x:rec
   where rec = replaceAt (n-1) new xs

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

isPerm m n = slist m == slist n 
    where slist = sort . toList 

cumsum = scanl1 (+)
maxOn f = maximumBy (compare `on` f)
-- apply f n times to x
fpow n f x = iterate f x !! n

isCoprime a b = gcd a b == 1

-- sumBools = foldl (flip ((+) . fromEnum)) 0

disquote = map (filter (/= '"')) 
decsv = map (split ',')
preproc file = disquote . sort . concat . 
                decsv $ lines file

filterTilFalse _ [] = []
filterTilFalse f xs = if f (last xs) then xs 
                      else filterTilFalse f (init xs)
