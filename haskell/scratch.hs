nand True True = False
nand _    _    = True

--- and is double nand: nand of nand 
and' a b = nand self self
        where self = nand a b

--- or is triple nand: nand of twice left nand and twice right nand
or' a b = nand aa bb
      where 
        aa = nand a a
        bb = nand b b

-- not is single nand: nand of twice self 
not' a = nand a a

-- xor is quadruple nand: nand of left-nand nand and right-nand nand
xor a b = nand a_ab ab_b
    where
        ab = nand a b
        a_ab = nand a ab
        ab_b = nand ab b


nor a b = nand aabb aabb
    where
        aa = nand a a 
        bb = nand b b
        aabb = nand aa bb 



main = do
    print $ nor True True
    print $ nor True False
    print $ nor False True
    print $ nor False False
