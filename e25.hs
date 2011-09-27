-- 4782
import Data.Array.IO

calcFib [] = [1]
calcFib [1] = [1,1]
calcFib r =
    let fn = sum $ take 2 $ reverse r
        in r ++ [fn]
		
genFib a = let fn = calcFib a in (length fn, last fn) : (genFib fn)
	
		
main = putStrLn $ show $ take 1 $ dropWhile (\(a,b) -> (length . show $ b) < 1000) $ genFib []