{--
    41063625 (345^3), can be permuted to produce two other cubes: 
    56623104 (384^3) and 66430125 (405^3). 
    In fact, 41063625 is the smallest cube with === 3 cube permutations

    Find the smallest cube for which exactly 5 permutations are cube.
--}

-- Just going to stringify this time

-- TODO: speed up my `group` 
import Utils (find,sort,group)


solve i n = root^3
    where 
        -- start at zero cos we're reusing indices for roots
        -- TODO: dynamically expand if search empty
        cubes = map (^3) [0..10^n]
        -- into canonical form
        canonicals = map (sort . show) cubes
        -- instead of permuting, just collect canonicals
        -- and get groups of size i
        targetLengthGroups = filter ((==i) . length) $ group canonicals
        -- each group has identical members so head, then min
        canonicalSolution = minimum $ map head targetLengthGroups
        -- go back and get the index, which is the root of our solution
        root = find canonicalSolution canonicals

-- need more headroom to find 5 permutations
answer = solve 5 4

main = do
    print $ solve 3 3 == 41063625
    print $ answer 
