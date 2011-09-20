candidates = 2 : [3,5..]

squareRoot :: Integer -> Integer
squareRoot x =
   floor $ sqrt $ fromInteger x

getPrimeFactors_ _ 1 = []
getPrimeFactors_ (d:c) number
   | number `mod` d == 0 = d : getPrimeFactors_ (d:c) (number `div` d)
   | otherwise = getPrimeFactors_ c number

getPrimeFactors n =
   getPrimeFactors_ candidates n

main =
   putStrLn $ show $ getPrimeFactors 600851475143
