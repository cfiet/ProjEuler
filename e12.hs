import Data.List
import qualified Data.Map as M

triangleNum n = n*(n+1) `div` 2

candidates = 2 : [3,5..]

squareRoot :: Integer -> Integer
squareRoot x =
   floor $ sqrt $ fromInteger x

getPrimeFactors_ _ 1 = []
getPrimeFactors_ [] _ = []
getPrimeFactors_ (d:c) number
   | number `mod` d == 0 = d : getPrimeFactors_ (d:c) (number `div` d)
   | otherwise = getPrimeFactors_ c number

getPrimeFactors n =
   (getPrimeFactors_ [2..n] n)

trianglePrimeDivisors :: Int -> [Int]
trianglePrimeDivisors n
	| n `mod` 2 == 0 = (getPrimeFactors $ n `div` 2) ++ (getPrimeFactors $ n+1)
	| otherwise = (getPrimeFactors n) ++ (getPrimeFactors $ (n+1) `div` 2)

countOccurences :: Int -> M.Map Int Int -> M.Map Int Int
countOccurences number acc =
	case (M.lookup number acc) of
		Nothing -> M.insert number 1 acc
		Just x -> M.insert number (x+1) acc
	
	
divisorsCount :: [(Int,Int)] -> Int
divisorsCount [] = 1
divisorsCount ((_,v):r) = (v+1) * divisorsCount r
	
trianglePrimeDivisorsCount n =
	divisorsCount . M.toList $ foldr (countOccurences) (M.empty) (trianglePrimeDivisors n)

main = do
	let n = head $ dropWhile (\x -> (trianglePrimeDivisorsCount x) < 500) [100..]
	putStrLn . show . triangleNum $ n