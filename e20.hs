import Char
import List

testMulti' [] = 1
testMulti' [n] = n
testMulti' p =
    let sp = p
        (h:t) = sp
        in h * (last t) * testMulti' (init t)
		
testMulti n = testMulti' $ sort n

fact n =
	testMulti [2..n]


main = putStrLn $ show $ sum $ map (\x -> (ord x) - (ord '0')) $ show $ fact 100