import Data.List

aToI :: String -> Integer
aToI s =
	read s


main = do
    s <- readFile "e13.txt"
    let l = take 10 $ show $ sum $ map aToI $ lines s in putStrLn $ show $ l
	
