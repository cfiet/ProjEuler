import Data.Array.IO

searchLimit = 1000000

calcSeq n
	| n `mod` 2 == 0 = n `div` 2
	| otherwise = 3 * n + 1

replaceNum arr nnum ndist nparent =
	do
		writeArray arr (nnum,1) nparent
		writeArray arr (nnum,2) ndist
		return ()

calculateDistance :: IOUArray Int Int -> Integer -> IO (Int)
calculateDistance arr num =
	do
		n <- if num <= searchLimit then readArray arr (fromInteger num) else return (-1)
		distance <- case n of
						-1 -> do
							d <- calculateDistance arr $ calcSeq num
							return (d+1)
						otherwise -> return n
		if num <= searchLimit then writeArray arr (fromInteger num) distance else return ()
		return distance

getMaxDistance :: IOUArray Int Int -> (Integer,Int) -> [Integer] -> IO (Integer,Int)
getMaxDistance arr max [] = return (max)
getMaxDistance arr (maxIndex, maxValue) (a:x) = do
	d <- calculateDistance arr a
	let newMax = if d > maxValue then (a,d) else (maxIndex, maxValue)
		in getMaxDistance arr newMax x
		
main = do
	p <- newArray (1, 1000000) (-1) :: IO (IOUArray Int Int)
	writeArray p 1 1
	d <- getMaxDistance p (0,0) [999999, 999997..500000]
	putStrLn $ show d


