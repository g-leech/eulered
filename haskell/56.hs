{-
    googol = (10^100) 
    100^100 = 10^200 
    Despite their size, the sum of the digits in each number is only 1.

    Considering natural numbers a^b, where a, b < 100, what is the maximum digital sum?
-}
import Utils (toList)

cap = 100

digitSum x = sum $ toList x
-- Strat 1: simple double loop, b to a halves it
answer = maximum [digitSum (a^b) | 
                    a<-[1..cap-1], b<-[1..a]]

main = do
    print $ answer