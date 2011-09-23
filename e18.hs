import Data.Array.IO
import Data.String.Utils
import System.IO

maxLevels = 15
numItems = maxLevels * (maxLevels+1) `div` 2

aToI :: String -> Int
aToI s =
    read s

initArray fileName = do
    fc <- readFile fileName
    let l = map (aToI) $ foldl1 (++) $ map (split " ") $ split "\n" fc
        in return l

treeElemIndex level n =
    level * (level-1) `div` 2 + n

treeElem arr level n
    | n > 0 && n <= level = do
        v <- readArray arr $ treeElemIndex level n
        return v
    | otherwise = return 0

getParents arr level n = do
    l <- treeElem arr (level-1) (n-1)
    r <- treeElem arr (level-1) n
    return [l, r]

findBiggest :: IOUArray Int Int -> Int -> Int -> Int -> IO (Int)
findBiggest arr max level n
    | level > maxLevels = return max
    | n > level = do
        rb <- findBiggest arr max (level+1) 1
        return rb
    | otherwise = do
        p <- getParents arr level n
        c <- treeElem arr level n
        putStrLn $ "Trying: " ++ (show (level,n,c)) ++ " max is: " ++ (show max)
        let bp = maximum p 
            v = bp+c
            newMax = maximum [max,v]
            in do 
                writeArray arr index (v)
                rb <- findBiggest arr newMax level (n+1)
                return rb
    where index = treeElemIndex level n

main = do
    arrInit <- initArray "e18.txt"
    putStrLn $ show arrInit
    arr <- newListArray (1, numItems) arrInit :: IO (IOUArray Int Int)
    biggest <- findBiggest arr 0 0 1
    putStrLn $ show biggest
