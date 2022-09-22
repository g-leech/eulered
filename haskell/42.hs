{-
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position 
and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. 
If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
-}

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Utils (split, sort, preproc)


alpha = '0':['A'..'Z']
num c = fromMaybe (-1) $ elemIndex c alpha
numsum word = sum $ map num word

triangle n = (n * (n+1)) `div` 2
triangles = map triangle [1..]
isTriangleWord w = s `elem` (takeWhile (<=s) triangles)
        where s = numsum w

sumbools = foldl (flip ((+) . fromEnum)) 0
answer txt = sumbools . map isTriangleWord $ preproc txt


main = do
    print $ isTriangleWord "SKY"
    raw <- readFile "data/p042_words.txt" 
    print $ answer raw