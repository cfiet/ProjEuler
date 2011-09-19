isDiv :: Integer -> Integer -> Bool
isDiv a b =
	b `mod` a == 0

isDiv3 :: Integer -> Bool
isDiv3 = isDiv 3

isDiv5 :: Integer -> Bool
isDiv5 = isDiv 5

main =
	putStrLn $ show $ sum [x | x <- [1..999], (isDiv3 x) || (isDiv5 x)]