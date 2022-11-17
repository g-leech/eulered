import Data.Bits
import Data.Char
import Data.List
import Data.Ord (comparing)
 
keys = [ [a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122] ]
allAlpha = all (\k -> let a = ord k in (a >= 32 && a <= 122))
howManySpaces = length . filter (==' ')
 
main = do
    s <- readFile "data/p059_cipher.txt"
    let 
        cipher = (read ("[" ++ s ++ "]") :: [Int])
        decrypts = [ map chr (zipWith xor (cycle key) cipher) | key <- keys ]
        alphaDecrypts = filter allAlpha decrypts
        message = maximumBy (comparing howManySpaces) alphaDecrypts
        asciisum = sum (map ord message)
    print message
