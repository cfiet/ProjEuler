import Data.Char
import Data.List

isProdPandigital n a b = (sort $ sa ++ sb ++ sc) == (foldr1 (++) $ map show [1..n])
    where sa = show a
          sb = show b
          sc = show (a*b)

main = 
    let checkPadigital = (\(a,b) -> isProdPandigital 9 a b)
        candidates = [(x,y) | x <- [1..10^5], y <- [1..10^5 `div` x], x < y]
        in putStrLn $ show $ sum $ nub $ map (\(a,b) -> a*b) $ filter checkPadigital candidates