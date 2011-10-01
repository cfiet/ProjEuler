import Data.Array.IO

o = [1,3..1000]
aVals = o ++ (map (*(-1)) o)

limitSize = 1000000
arraySize = (limitSize `div` 2) - 1
sieveLimit = ceiling $ sqrt $ fromInteger arraySize

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

smallPrimes arr i
    | n > 1000 = return []
    | otherwise = do
        b <- readArray arr i
        r <- smallPrimes arr (i+1)
        let v = if b then [n] else []
            in return (v ++ r)
    where n = indexToNumber i

combinations [] _ = []
combinations (x:xs) l =
    (map (\a -> (x,a))) l ++ (combinations xs l)

checkPrimes arr p n
    | i > 0 = do
        b0 <- readArray arr i
        if b0 then do
            n1 <- checkPrimes arr p (n+1)
            return n1
        else
            return (n-1)
    | otherwise = return n
    where (a,b) = p
          val = n^2 + a*n + b
          i = numberToIndex val

findBest :: IOUArray Integer Bool -> (Integer, (Integer, Integer)) -> [(Integer, Integer)] -> IO (Integer, (Integer, Integer))
findBest _ bst [] = return bst
findBest arr (bstn, bstp) (x:xs) = do
    n <- checkPrimes arr x 0
    newbst <- if n > bstn then return (n, x) else return (bstn, bstp)
    ret <- findBest arr newbst xs
    return ret

main = do
    arr <- newArray (0, arraySize) $ True :: IO (IOUArray Integer Bool)
    sieve arr 0
    sp <- smallPrimes arr 0
    bst <- let p = combinations aVals sp in findBest arr (0, (0,0)) p
    let (_,(a,b)) = bst in putStrLn $ show $ a*b
