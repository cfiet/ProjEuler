-- We could of course generate permutations of these numbers, than
-- sort them and select the millionth, but it would just be boring.
--
-- Instead, we should notice that:
-- There are n! permutations of n digits. Therefore, for these 10
-- digits, we have 10! permutations avalible, it's more than 3 mln
-- so the problem is solvable.
-- Now, let's select the 0 as the first digit, since it will be the
-- lowest thus giving us the lowest permutations. How many permutations
-- of the rest digits we have? 9! = 362880, not even close to 1mln.
-- So we select the next number: 1, as the first number. We skipped
-- 362880 permutations, now we have 9! + 9! = 2 * 9! permutations
-- we could skip. It's still less then million. So it's not our
-- number. We select 2, now we have 1088640 permutations we could have
-- skipped (the 1088640th is the biggest permutation starting with 2, 
-- so it's 2987654310). Now we know it's ont of the 9! permutations
-- starting with two, to be exact it is (1000000 - 2*9!)th permutation
-- of (0,1,3,4,5,6,7,8,9). We can repeat the above steps but now looking
-- for 274240th permutation of reminding digits.
--
-- To generalise, we have:
-- L(9) = 1000000
-- P(9) * 9! <= L(9)
-- (P(9) + 1) * 9! > L(9)
-- and then:
-- 
--  / L(N) = L(N+1) - P(N+1) * (N+1)! for N < 9
-- |
-- |  for N <= 9:
-- |  P(N) * N! <= L(N)
-- |  (P(N)+1) * N! > L(N)
-- |  P(N) < N
--  \ P(N) >= 0
--
-- No we generate (D(9), D(8), D(7), D(6) ... D(0)) according to:
-- each P(N) is the index of the number in the table T(N) of numbers not
-- used untill step n, so:
-- T(9) = [0, 1, 2 ... 9], P(9) = 2 --> D(9) = 2
-- T(8) = [0, 1, 3 ... 9], P(8) = 6 --> D(8) = 7
-- and so on.

splitMulti :: [Int] -> Int
splitMulti [] = 1
splitMulti [a] = a
splitMulti (x:xs) =
    let l = x * (last xs)
        r = splitMulti (init xs)
        in l * r

fact 1 = 1
fact n = splitMulti [2..n]

findPermut xs 0 = xs
findPermut toSelect whichPermut
    | n == 0 = []
    | otherwise = d : (findPermut [x | x <- toSelect, x /= d] (whichPermut - p*factN))
    where n = (length toSelect)
          factN = fact (n-1)
          rN = whichPermut `mod` factN
          pN = whichPermut `div` factN
          p = if rN /= 0 then pN else (pN-1)
          d = toSelect !! p
		
main = putStrLn $ foldl1 (++) $ map (show) $ findPermut [0..9] 1000000