{-
  take a text file, 
  convert the bytes to ASCII, 
  XOR each byte with a given value from a secret key.
  
  Our encryption key consists of three lower case characters. 
  file contains the encrypted ASCII codes, 
  the plain text is English, 
  decrypt the message and find the sum of the ASCII values in the original text.
-}
import Utils (split,count,subIndex,rawSubIndex)
import Data.Char (ord,chr)
import Data.Bits (xor)
import Data.Maybe (isNothing)

-- Strat 1: obviously just look for "the" crib. Space is crucial
cribs = map (map ord) ["the ", "is "]

r = read :: [Char] -> Int
asciis f = map r $ split ',' f
ciphertext ciphernums = map chr ciphernums

min_ord = 97
max_ord = 122
allkeys = [[a,b,c] | let vals = [min_ord..max_ord],
        a<-vals, b<-vals, c<-vals]

findKeyWithCrib keys ciphered
    | cribFound = concat $ take 3 key
    | otherwise = findKeyWithCrib restkeys ciphered
    where
        (key, restkeys) = splitAt 1 keys
        key' = concat key
        xored = zipWith xor (cycle key') ciphered
        found crib = not $ isNothing (rawSubIndex crib xored)
        cribFound = all (==True) $ map found cribs

answer ciph = sum decrypt
    where 
        key = findKeyWithCrib allkeys ciph
        decrypt = zipWith xor key ciph

gimmetext cs key = ciphertext (zipWith xor truekey cs)
    where truekey = cycle key

main = do
    raw <- readFile "data/p059_cipher.txt" 
    let ciphernums = asciis raw
    let key = findKeyWithCrib allkeys ciphernums
    print $ gimmetext ciphernums key