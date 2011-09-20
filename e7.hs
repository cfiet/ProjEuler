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
generatePrimes_ primes (x:c)
   | (isPrime primes x) = x : generatePrimes_ (x:primes) c
   | otherwise = generatePrimes_ primes c

generatePrimes_ currentPrimes [] =
	currentPrimes
		
		
main =
	putStrLn $ show $ last $ take 10001 $ generatePrimes
