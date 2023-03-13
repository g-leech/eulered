{--
    41063625 (345^3), can be permuted to produce two other cubes: 
    56623104 (384^3) and 66430125 (405^3). 
    In fact, 41063625 is the smallest cube with === 3 cube permutations

    Find the smallest cube for which exactly 5 permutations are cube.
--}
-- TODO: speed up my `group` 
import Utils (find,sort,group)


-- Strat 1: generate cubes, sort digits for a canonical form, 
-- find group of canonicals with size 5.
-- Just going to stringify this time
solveN i n = (canonicalSolution, canonicals) 
    where 
        -- start at zero cos we're reusing indices for roots
        cubes = map (^3) [0..10^n]
        -- into canonical form
        canonicals = map (sort . show) cubes
        -- instead of permuting, just collect canonicals
        -- and get groups of size i
        targetLengthGroups = filter ((==i) . length) $ group canonicals
        -- each group has identical members so head, then min
        canonicalSolution = map head targetLengthGroups 

expandingSearch i n
    | canonicalSolution == [] = expandingSearch i (n+1)
    | otherwise = (canonicalSolution, canonicals)
    where 
        (canonicalSolution, canonicals) = solveN i n
        
-- Iteratively expand the search. 
-- Prevents false positives and weasel parameter on number of cubes to search over
solve i = root^3
    where
        (canonicalSolution, canonicals) = expandingSearch i 2
        -- go back and get the index, which is the root of our solution
        root = find (minimum canonicalSolution) canonicals

-- need more headroom to find 5 permutations
answer = solve 5

main = do
    print $ solve 3 == 41063625
    print $ answer 
