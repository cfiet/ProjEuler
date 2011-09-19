sqOfS = (sum [1..100]) * (sum [1..100])
sOfSq = sum [x*x | x <- [1..100]]

main =
	putStrLn $ show $ (sqOfS - sOfSq)