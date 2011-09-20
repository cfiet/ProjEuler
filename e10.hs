import Data.Array.IO

limitSize = 2000000
--sieveLimit = ceiling $ sqrt $ fromInteger limitSize


sieveRemove arr s i
	| i+s <= limitSize = do 
				writeArray arr (i+s) False 
				sieveRemove arr s (i+s)
	| otherwise = return ()

sieve arr i
	| i <= limitSize = do
					v <- readArray arr i
					case v of
						True -> sieveRemove arr i i
						_ -> return ()
					sieve arr (i+1)
	| otherwise = return ()



main = do
	arr <- newArray (2, limitSize) $ True :: IO (IOUArray Integer Bool)
	sieve arr 2
	e <- getElems arr
	let s = map (\(x,y) -> x) $ filter (\(x,y) -> y == True) $ zip [2..] e in 
		putStrLn $ show $ sum s
	return ()
