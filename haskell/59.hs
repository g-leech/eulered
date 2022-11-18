{-
  take a text file, 
  convert the bytes to ASCII, 
  XOR each byte with a given value from a secret key.
  
  Our encryption key consists of three lower case characters. 
  file contains the encrypted ASCII codes, 
  the plain text is English, 
  decrypt the message and find the sum of the ASCII values in the original text.
-}
-- Hidden lines: 16
import Utils (split,count,rawSubIndex)
import Data.Char (ord,chr)
import Data.Bits (xor)
import Data.Maybe (isNothing)

-- Strat 1: obviously just look for "the" crib. Space after is crucial
cribs = map (map ord) ["the ", "is "]

findPlaintext keys ciphered
    | cribFound = xored 
    | otherwise = findPlaintext restkeys ciphered
    where
        (key, restkeys) = splitAt 1 keys
        xored = zipWith xor (cycle $ concat key) ciphered
        found crib = not $ isNothing (rawSubIndex crib xored)
        cribFound = all (==True) $ map found cribs

(min_ord,max_ord) = (97, 122)
allkeys = [[a,b,c] | let vals = [min_ord..max_ord],
            a<-vals, b<-vals, c<-vals]
answer ciph = findPlaintext allkeys ciph

main = do
    let r = read :: [Char] -> Int
    let asciis f = map r $ split ',' f
    let toText nums = map chr nums
    
    raw <- readFile "data/p059_cipher.txt" 
    let ciphernums = asciis raw
    print $ toText $ answer ciphernums