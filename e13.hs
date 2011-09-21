import Data.Array.IO
import System.IO
import System.IO.Error

numOfNumbers = 100
numOfDigits = 11
numOfCarry = 3

aToI :: String -> Int
aToI s =
	read s
	
hGetCharOrNull h = hGetChar h `catch` eofHandler where
	eofHandler e = if isEOFError e then return '\0' else ioError e
	
increaseAtPosition arr pos inc = do
		posValue <- readArray arr pos
		let nValue = posValue + inc in
			setAtPosition arr pos nValue
	
setAtPosition arr pos value
	| value < 10 = writeArray arr pos value
	| otherwise = do
		increaseAtPosition arr (pos-1) 1
		writeArray arr pos (value `mod` 10)
	
readDigit hndl arr pos = do
	n <- hGetCharOrNull hndl
	case n of
		'\0' -> return ()
		'\n' -> readDigit hndl arr (numOfCarry-1) 
		_ -> interpretDigit hndl arr pos n

interpretDigit hndl arr pos n
	| pos > numOfDigits+numOfCarry-1 =  readDigit hndl arr (pos+1)
	| otherwise = let i = aToI [n] in do
		increaseAtPosition arr pos i
		readDigit hndl arr (pos+1) 
			
main = do
	arr <- newArray (0, (numOfDigits+numOfCarry-1)) 0 :: IO (IOUArray Int Int)
	withFile "e13.txt" ReadMode $ (\h -> readDigit h arr (numOfCarry-1))
	l <- getElems arr
	putStrLn $ foldr (++) [] $ map (show) $ take 10 l
	return ()