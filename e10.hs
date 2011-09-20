import Data.Array.IO

limitSize = 2000000
arraySize = (limitSize `div` 2) - 1
sieveLimit = ceiling $ sqrt $ fromInteger arraySize
--sieveLimit = ceiling $ sqrt $ fromInteger limitSize

indexToNumber i =
	3 + 2*i

numberToIndex n =
	(n-3) `div` 2

sieveRemove arr s i
	| i <= arraySize = do
		writeArray arr i False
		sieveRemove arr s (i+s)
	| otherwise = return ()

sieve arr i
	| i <= numberToIndex sieveLimit =
		let n = indexToNumber i in do
			v <- readArray arr i
			case v of
				True ->sieveRemove arr n (numberToIndex $ n*n)
				_ -> return ()
			sieve arr (i+1)
	| otherwise = return ()

sumA arr i s
	| i <= arraySize = do
		v <- readArray arr i
		let ns = case v of {True -> (s+3+2*i); False -> s;} in sumA arr (i+1) ns
	| otherwise = return s

main = do
	arr <- newArray (0, arraySize) $ True :: IO (IOUArray Integer Bool)
	sieve arr 0
	s <- sumA arr 0 0
	putStrLn . show $ s
