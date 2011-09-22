import Data.Array
import Data.String.Utils
import System.IO

gridRows = 20
gridCols = 20
prodEl = 4

aToI :: String -> Int
aToI s =
    read s

elXY arr (x,y)
    | x >= 0 && x < gridCols && y >= 0 && y < gridRows = (!) arr (y*20+x)
    | otherwise = 0

findArrMax :: Array Int Int -> (Int,Int) -> Int -> Int
findArrMax arr (x,y) currentMax
    | x == gridCols && y == gridRows = currentMax
    | x == gridCols = findArrMax arr (0,y+1) currentMax
    | otherwise = let l = leftProd arr (x,y)
                      d = downProd arr (x,y)
                      dl = diagLeftProd arr (x,y)
                      dr = diagRightProd arr (x,y)
                      max = maximum $ currentMax : l : d : dl : dr : []
                      in  findArrMax arr (x+1,y) max
    where xMax = gridCols - 1
          yMax = gridRows - 1

calcProd :: (Int -> (Int,Int)) -> (Array Int Int -> (Int, Int) -> Int)
calcProd transform arr (x,y) =
    foldl (*) 1 $ map (elXY arr) $ map (transform) [0..(prodEl-1)]


leftProd :: Array Int Int -> (Int, Int) -> Int
leftProd arr (x,y) = calcProd (\i -> ((x-i), y)) arr (x,y)
downProd arr (x,y) = calcProd (\i -> (x, (y+i))) arr (x,y)
diagLeftProd arr (x,y) = calcProd (\i -> ((x-i), (y+i))) arr (x,y)
diagRightProd arr (x,y) = calcProd (\i -> ((x+i), (y+i))) arr (x,y)



main = do
    fc <- readFile "e11.txt"
    let ia = map aToI $ filter (\x -> length x > 0) $ split " "  $ map (\x -> if x == '\n' then ' ' else x) fc 
        arr = listArray (0,(gridRows*gridCols-1)) ia :: Array Int Int
        in putStrLn $ show $ findArrMax arr (0,0) 0
