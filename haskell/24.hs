{-
    3124 is one possible permutation of the digits 1, 2, 3 and 4. 
    If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. 

    The lexicographic permutations of 0, 1 and 2: 012   021   102   120   201   210

    What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-}

-- Strat 1: generate em with iteration-recursion and index to 999999
-- nPermutations is fac n so 10 does it.
n = 1000000

-- One tuple per x; x is what's picked
picks [] = []
picks (x:xs) = [(xs,x)] ++ rest
            where rest = [(x:ys,y) | (ys,y) <- picks xs]

perms [] = [[]]
perms xs = do
    (ys,x) <- picks xs
    zs     <- perms ys
    return (x:zs)
nthPerm xs = (perms xs) !! (n - 1)

answer = nthPerm [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

main = do
    print $ perms [0,1,2] == [[0,1,2],[0,2,1],[1,0,2],[1,2,0],[2,0,1],[2,1,0]]
    print $ answer