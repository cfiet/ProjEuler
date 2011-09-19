limit = 999

progNum :: Integer -> Integer
progNum x = limit `div` x

progSum :: Integer -> Integer
progSum x =
	let n = progNum x in
	n * (n+1) * x `div` 2

main =
	putStrLn $ show $ (progSum 3) + (progSum 5) - (progSum 15)