import System.IO
import Data.String.Utils

num (0,0,0) = []
num (0,0,1) = "one"
num (0,0,2) = "two"
num (0,0,3) = "three"
num (0,0,4) = "four"
num (0,0,5) = "five"
num (0,0,6) = "six"
num (0,0,7) = "seven"
num (0,0,8) = "eight"
num (0,0,9) = "nine"
num (0,1,0) = "ten"
num (0,1,1) = "eleven"
num (0,1,2) = "twelve"
num (0,1,3) = "thirteen"
num (0,1,4) = "fourteen"
num (0,1,5) = "fifteen"
num (0,1,6) = "sixteen"
num (0,1,7) = "seventeen"
num (0,1,8) = "eighteen"
num (0,1,9) = "nineteen"
num (0,2,0) = "twenty"
num (0,3,0) = "thirty"
num (0,4,0) = "forty"
num (0,5,0) = "fifty"
num (0,6,0) = "sixty"
num (0,7,0) = "seventy"
num (0,8,0) = "eighty"
num (0,9,0) = "ninety"

num (h,d,s)
    | number >= (1,0,0) = (num (0,0,h)) ++ case d+s of {0->"hundred"; _->"hundredand"} ++ (num (0,d,s))
    | number > (0,2,0) = (num (0,d,0)) ++ (num (0,0,s))
    where number = (h,d,s)

getNumbers = do
    f <- readFile "e17.txt"
    let ln = map (\x -> filter (/=' ') $ filter (/='-') x ) $ split "\n" f
        in return (ln)
	
	
main = do
    --f <- getNumbers
    let words = map (num) $ [(x,y,z) |x <- [0..9], y <- [0..9], z <- [0..9]]
        allWords = filter (\x -> length x > 0) $ words ++ ["onethousand"]
        nums = foldl (++) "" $ allWords 
        --baseNums = foldl (++) "" $ f
        --cmp = zip allWords f
		in putStrLn $ show $ length nums
