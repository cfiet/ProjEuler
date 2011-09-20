main =
   let x = [[a, b, 1000-a-b] | a <- reverse [1 .. 998],
                               b <- reverse [1 .. 999 - a],
                               let c = 1000 - a - b in
                               b*b == a*a + c*c
                               ||
                               a*a == b*b + c*c]
   in
   putStrLn $ show $ foldr (*) 1 $ head x
