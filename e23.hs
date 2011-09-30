import Data.Array.Unboxed
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

findAbundant  :: [(Integer, [Integer])] -> [Integer]
findAbundant [] = []
findAbundant ((n,d):xs) =
    let snd = sum d
        e = if snd > n then [n] else []
        in (e ++ findAbundant xs)

checkAbundantSum :: Array Integer Bool -> [Integer] -> Integer -> Bool   
checkAbundantSum arr [] _ = False
checkAbundantSum arr (x:xs) n = (arr ! (n-x)) || (checkAbundantSum arr xs n)

checkNonAbundantSummable :: Array Integer Bool -> [Integer] -> [Integer] -> [Integer]
checkNonAbundantSummable _ _ [] = []
checkNonAbundantSummable arr amb (x:xs) =
    let c = [e | e <- amb, e < x]
        h = if not $ checkAbundantSum arr c x then [x] else []
        in h ++ (checkNonAbundantSummable arr amb xs)
        
main = do
    let im = M.fromList [(1, [1]), (2, [1]), (3,[1])]
        d = findAbundant $ M.toList $ generateDivisors im [4..20161]
        -- Cache the numbers in array so we have O(1) access rather than O(n)
        arr = listArray (1, 20161) [x `elem` d | x <- [1..20161]] :: Array Integer Bool
        -- All the even numbers greater than 48 can be written as a sum of abundant
        -- numbers. All the numbers greater than 20161 can be written as a sum of
        -- abundant number.
        ns = sum $ checkNonAbundantSummable arr d $ [1..48] ++ [49,51..20162]
        in putStrLn $ show ns