-- http://en.wikipedia.org/wiki/Repeating_decimal#Reciprocals_of_composite_integers_coprime_to_10

firstPrimes [] = []
firstPrimes (x:xs) = x : (firstPrimes [n | n <-xs, n `mod` x /= 0])

lenOfCycle denom
    | n == [] = 0
    | otherwise = head n
    where n = dropWhile (\x -> 10^x `mod` denom /= 1) [1..denom]
    
filterMax (a, b) (ma, mb) = if b > mb then (a, b) else (ma, mb)
    
main = putStrLn $ show $ foldr filterMax (0,0) $ map (\x-> (x, lenOfCycle x)) $ firstPrimes [2..1000]
--map (lenOfCycle) [7, 11, 17]