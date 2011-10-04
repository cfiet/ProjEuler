import Control.Monad
import Data.Array.IO
import Data.List

limitSize = 1000000
arraySize = (limitSize `div` 2) - 1
sieveLimit = ceiling $ sqrt $ fromInteger arraySize

indexToNumber i =
	3 + 2*i

numberToIndex n = (n-3) `div` 2

aToI :: String -> Integer
aToI s = read s
    
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

circ' _ 1 = []
circ' (x:xs) n = [c] ++ circ' c (n-1)
    where c = xs ++ [x]
    
circ a = circ' a (length a)

checkPrimes :: IOUArray Integer Bool -> [Integer] -> IO Bool
checkPrimes _ [] = return True  
checkPrimes arr (x:xs)
    | x `mod` 2 == 0 = return False
    | otherwise = do
        b <- readArray arr (numberToIndex x)
        if b then do
            d <- checkPrimes arr xs
            return d
        else
            return False
    
getCirculars :: IOUArray Integer Bool -> Integer -> IO [Integer]
getCirculars arr i
    | i >= arraySize = return []
    | otherwise = do
        isPrime <- readArray arr i
        h <- if isPrime then do
                        b <- checkPrimes arr p
                        return (if b then [n] else [])
             else do
                        return []
        t <- getCirculars arr (i+1)
        return $ h ++ t
    where n = indexToNumber i
          p = map aToI $ circ $ show n

main = do
	arr <- newArray (0, arraySize) $ True :: IO (IOUArray Integer Bool)
	sieve arr 0
	s <- getCirculars arr 0
	putStrLn . show $ length $ 2 : s
