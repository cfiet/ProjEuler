import Char
import Data.List
import Data.String.Utils
import System.IO

getNameList :: IO [String]
getNameList = do
    s <- readFile "e22.txt"
    let proc = quickSort $ split "," $ filter (/='\"')  s
        in return proc

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (a:ls) = quickSort smaller ++ (a : quickSort bigger)
    where smaller = [e | e <- ls, e <= a]
          bigger = [e | e <- ls, e > a]

wordValue :: String -> Int
wordValue s =
    sum $ map (\x -> 1 + (ord x) - (ord 'A')) s

listWordValue _ [] = 0
listWordValue i (a:ls) =
    i * (wordValue a) + (listWordValue (i+1) ls)

main :: IO ()
main = do
    p <- getNameList
    putStrLn $ show $ listWordValue 1 p
