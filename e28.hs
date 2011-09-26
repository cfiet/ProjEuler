-- The diagonals of the 4-th level spiral are the following:
--
--		43  .  .  .  .  . 37
--       . 21  .  .  . 17  .
--		 .  .  7  .  5  .  .
--       .  .  .  1  .  .  .
--       .  .  9  .  3  .  .
--       . 25  .  .  . 13  .
--      19  .  .  .  .  .  31
--
-- Treating them as one set doesn't reveal much: 
--		1 (+2) 3 (+2) 5 (+2) 7 (+2) 9 (+4) 13 (+4) 17 (+4) ...
-- but you can see, that each 'step' is repated 4 times. Therfore you can try
-- to treat them as four separate series:
--    diag1 = 1, 3, 13, 31 ...
--    diag2 = 1, 5, 17, 37 ...
--    diag3 = 1, 7, 21, 43 ...
--    diag4 = 1, 9, 25, 49 ...
--
-- Now we can try to get the formula for each of this series. Let's try with
-- quadratic function (came to me after plotting the series) and diag1:
--   d1(n) = an^2 + bn + c
--
-- For the diag1, we get
--
--     /  a +  b + c =  1
--   <|  4a + 2b + c =  3
--     \ 9a + 3b + c = 13  
--
-- Solving this, we get:
--       a = 4, b = -10, c = 7
--
-- Checking that d1(4) == 31, it seems to work.
-- After repeating for all the series we get:
--       diag1 --> a = 4, b = -10, c = 7
--       diag2 --> a = 4, b =  -8, c = 5
--       diag3 --> a = 4, b =  -6, c = 3
--       diag4 --> a = 4, b =  -4, c = 1
--
-- Now we have the formula that generates numbers on each of the levels in O(1).
--
-- Last thing we should notice is that spiral of level n has 2n-1 numbers on the
-- side:
--		s(1) = 1
--      s(2) = 3
--      s(3) = 5
--		 ...   ...
--
-- So from: s(x) = 2x - 1 = 1001 we get that: x = 501, so we need to calculate
-- the sum of numbers on diagonal of spiral of 501st level.

diag :: (Int, Int, Int) -> (Int -> Int)
diag (a,b,c) = (\n -> a*n*n + b*n + c)

diag1 n = diag (4, -10, 7) n
diag2 n = diag (4, -8, 5) n
diag3 n = diag (4, -6, 3) n
diag4 n = diag (4, -4, 1) n

diagNumbers :: Int -> [Int]
diagNumbers 1 = [1]
diagNumbers n = (diagNumbers (n-1)) ++ [diag1 n, diag2 n, diag3 n, diag4 n]

main = putStrLn $ show $ sum $ diagNumbers 501