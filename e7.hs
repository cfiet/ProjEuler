candidates :: [Integer]
candidates = 2 : [3,5..]

isPrime :: [Integer] -> Integer -> Bool
isPrime [] _ =
	True
isPrime currentPrimes number =
	foldr (&&) True (map (\x -> number `mod` x /= 0) currentPrimes)

generatePrimes :: [Integer]
generatePrimes =
	generatePrimes_ [] candidates

generatePrimes_ :: [Integer] -> [Integer] -> [Integer]
generatePrimes_ currentPrimes (x:candidates) =
	if (isPrime currentPrimes x) then 
		generatePrimes_ (currentPrimes ++ [x]) candidates
	else
		generatePrimes_ currentPrimes candidates
		
generatePrimes_ currentPrimes [] =
	currentPrimes
		
		
main =
	putStrLn $ show $ take 2 generatePrimes