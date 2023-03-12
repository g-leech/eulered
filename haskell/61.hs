{-
    Figurate numbers

    [8128, 2882, 8281]:
        1. cyclic, abcd -> cdxy
        2. Fits figurate 3-5
        3. a unique solution for (1) and (2)

Find sum of A

A = the only list of 6 unique cd-cyclic 4-digiters 
    which fits figurate 3-8 
-}

-- Strat 1: Gen all 4-digiters p3-8, filter on cyclic.
-- Strat 2: Gen all sets of 6 4-digit cyclic, filter on p3-8.

import Data.Set (toList, fromList, notMember)

cyclic x y = xcd == yab 
    where
        xcd = x `mod` 100
        yab = y `div` 100 

-- Strat 1
matchCyc x ys = filter (cyclic x) $ concatMap toList ys
remove x ys = filter (notMember x) ys
-- glorified nested for loop (but contraction mapping)
-- want one entry from each figurate level
-- want the final list to loop: `cyclic last first`
-- so we're paring down each set of figurates to just the ones which fit
-- TODO: learn metaprogramming
solve figSets = concat [ [f3s, sansF3s, c, d, e, final] 
                 | f3s <- toList $ head figSets,
                   sansF3s <- matchCyc f3s $ tail figSets,
                   let b = remove sansF3s $ tail figSets,
                   c <- matchCyc sansF3s b,
                   let ce = remove c b,
                   d <- matchCyc c ce,
                   let de = remove d ce,
                   e <- matchCyc d de,
                   let ee = remove e de,
                   final <- matchCyc e ee,
                   cyclic final f3s
                ]
figurates :: Int -> [Int]
figurates i = scanl (+) 1 [i-1, 2*i-3 ..]
figurates3to8 = map (boundSet . figurates) [3..8]
    where boundSet = fromList . takeWhile (< 10000) . dropWhile (< 1011)
answer = sum $ solve figurates3to8


main = do
    print $ answer




-- closed form, verbose. p_even_i = an(n-c) 
-- evenfig i n = n * (a*n-c)
--     where 
--         a = 1 + (i-4)`div` 2
--         c = (i-4) `div` 2

-- oddfig i n = n * (a*n+c) `div` 2
--     where 
--         a = 1 + (i-3) 
--         c = 4 - i 