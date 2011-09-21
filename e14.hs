import Data.Array.IO

searchLimit = 1000000

seq n
	| n `mod` 2 == 0 = n / 2
	| otherwise = 3 * n + 1


replaceNum arr nnum ndist nparent =
	do
		writeArray arr (nnum,1) nparent
		writeArray arr (nnum,2) ndist
		return ()





checkNum arr num dist =
	let nnum = seq n in
		if nnum == 1 then return ()
		else do
			odist <- readArray (nnum,2)
			if odist == -1 then
			else
				if odist < dist then replaceNum arr nnum (dist+1)



main = do
	p <- newArray ((1,1),(searchLimit, 2)) (-1) :: IO (IOUArray (Int,Int) Int)
	writeArray p (1,1) 0
	writeArray p (1,2) 1
	return ()


