import qualified Data.List as L
import qualified Data.Map as M

foldDivisors :: M.Map Integer [Integer] -> [Integer] -> Integer -> [Integer]
foldDivisors m [] a = []
foldDivisors m (k:ks) a
    | reminder == 0 = let dk = M.findWithDefault [] k m 
                          h = [k, (a `div` k)]
                          in h ++ dk ++ (foldDivisors m ks a)
    | otherwise = (foldDivisors m ks a)
    where reminder = a `mod` k

getDivisors :: M.Map Integer [Integer] -> Integer -> M.Map Integer [Integer]
getDivisors m a =
    let v = [k | k <- M.keys m, k > 1, k <= (floor . sqrt . fromInteger $ a)]
        w = if a `mod` 2 == 0 then v else [k | k <- v, k `mod` 2 /= 0]
        d = L.nub $ (1 : foldDivisors m w a)
        in M.insert a d m
 
generateDivisors m [] = m
generateDivisors m (x:xs) =
    let n = getDivisors m x
        in generateDivisors n xs

findAmicable :: M.Map Integer [Integer] -> [Integer]
findAmicable m
    | m == M.empty = []
    | otherwise = let nn = sum d
                      nd = M.findWithDefault [] nn m
                      rst = M.delete n m
                      (e, mm) = if n /= nn && (sum nd == n) then
                                    ([n, nn], M.delete nn rst)
                                else ([], rst)
                      h = if n == nn then [] else e
                      in (h ++ findAmicable mm)
                         
    where ((n,d):xs) = M.toList m

main = do
    let im = M.fromList [(1, [1]), (2, [1]), (3,[1])] 
        d = sum $ findAmicable $ generateDivisors im [4..10000]
        in putStrLn $ show d