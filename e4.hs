numToList_ :: Integer -> [Integer] -> [Integer]
numToList_ 0 l = l
numToList_ n l =
	numToList_ (n `div` 10) ((n `mod` 10) : l)
	
numToList i = numToList_ i []
	
isPalindrome :: [Integer] -> Bool
isPalindrome [] = True
isPalindrome (f:[]) = True
isPalindrome (f:r) =
    if f /= (last r) then False else isPalindrome $ init r
	
nums = [x*y | x <- [990,(990-11)..400], y <- [250..999], isPalindrome $ numToList $ x*y]
	
main =
    putStrLn $ show $ maximum nums