sieve = 2 : sieve_ [3,5..]
sieve_ (n:r) = n : (sieve_ $ filter (\x -> x `mod` n /= 0) r)

main =
	putStrLn . show . sum $ takeWhile (<2000000) sieve