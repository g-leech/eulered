{-
    Smallest cyclic number with cycle length 6

    It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits.

    Find the smallest positive integer x such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
-}
import Utils (fromList,toList,sort)

canon = sort . toList
allSame xs = and $ zipWith (==) cs (tail cs)
    where cs = map canon xs
series x = map (x *) [1..6]

answer = head . head $ filter allSame multiples
    where multiples = map series [1..]

main = do
    print $ answer