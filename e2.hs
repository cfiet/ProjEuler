-- Generate evbery 3rd Fibbonacci number
fib4 :: Integer -> Integer
fib4 1 = 2
fib4 2 = 8
fib4 x =
    let a = 4 * (fib4 (x-1))
        b = fib4 (x-2)
    in a + b

-- 33rd fibbonnacci number gets over 4mln, so we should sum all the
-- even numbers up to 32nd. Since every 3rd fibbonacci number is even,
-- we should sum first 33/3=11 number of fib4
main =
    putStrLn $ show $ sum [fib4 x | x <- [1..11]]