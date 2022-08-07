{-
    Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
    there are exactly 6 routes to the bottom right corner.

    https://projecteuler.net/project/images/p015.png

    How many such routes are there through a 20×20 grid?
-}

-- Strat 1: Model it. 2D array, recursive paths, check legal moves. exhausting.
{- 
    Strat 2: Ignore it, basically no structure needed.
    To get from 0,0 to n,n takes n downs and n rights. 
    Paths equal length, order irrelevant.
    Two options at each step and n steps, so 2*n total choices
-}

x = 20

fac n = foldl1 (\x y -> x*y) [1..n]
binom n k = permuts `div` (orders * spares)
            where permuts = fac n
                  orders = fac k
                  spares = fac (n-k)
brRoutes n = binom (2*n) n
answer = brRoutes 20

main = do
    print $ brRoutes 2 == 6
    print $ answer




{-
a = 20
goal n = (n-1, n-1)
inBounds end n = end <= goal n
down start end = end >= start -- `fst` is implicit
right start end = snd end >= snd start

legalMove :: (Int, Int) -> (Int, Int) -> Int -> Bool
legalMove start end n = inBounds end n && 
                        (down start end || 
                        right start end)
--legalPath start end n = inBounds end n && 

-}