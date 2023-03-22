{-
    There is only one Pythagorean triplet, {a, b, c}, for which a + b + c = 1000. 
    Find abc.
-}

isTriple a b c = c^2 == (a^2 + b^2)
triples l = head [ a*b*c | a<-[1..500], b<-[1..a],
            let c = l - a - b,
            isTriple a b c
         ] 

main = do
    -- print $ isTriple 3 4 5
    answer = triples 1000
    print $ answer