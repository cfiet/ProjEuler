
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
num (0,1,2) = "tvelwe"
num (0,1,3) = "thirteen"
num (0,1,4) = "fourteen"
num (0,1,5) = "fifteen"
num (0,1,6) = "sixteen"
num (0,1,7) = "seventeen"
num (0,1,8) = "eighteen"
num (0,1,9) = "nienteen"
num (0,2,0) = "twenty"
num (0,3,0) = "thirty"
num (0,4,0) = "forty"
num (0,5,0) = "fifty"
num (0,6,0) = "sixty"
num (0,7,0) = "seventy"
num (0,8,0) = "eigthy"
num (0,9,0) = "ninety"

num (h,d,s)
	| number > (1,0,0) = (num (0,0,h)) ++ "hundredand" ++ (num (0,d,s))
	| number > (0,2,0) = (num (0,d,0)) ++ (num (0,0,s))
	where number = (h,d,s)




main = do
	let nums = foldl (++) "" $ map (num) [(x,y,z) |x <- [1..9], y <- [1..9], z <- [1..9]]
		in putStrLn . show . length $ nums


