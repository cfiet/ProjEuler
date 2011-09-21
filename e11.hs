import Data.Array.IO
import Data.String.Utils
import System.IO

gridSize = (19,19)

aToI :: String -> Int
aToI s =
    read s

initArray_ arr (x,y) (a:r)
    | x < xLimit = do
        writeArray arr (x,y) a
        initArray_ arr (x+1, y) r
    | y < yLimit = initArray_ arr (0,y+1) (a:r)
    | otherwise = return ()
    where (xLimit, yLimit) = gridSize

initArray fn arr = do
    sArr <- readFile fn
    let l = map (aToI) $ foldr (++) [] $ map (split " ") (split "\n" sArr)
        in initArray_ sArr (0,0) l

main = do
    arr <- newArray ((0,0),gridSize) 0 :: IO (IOUArray (Int,Int) Int)
    initArray "e11.txt" arr
    return ()
