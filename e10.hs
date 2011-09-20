candidates :: [Integer]
candidates = [2..2000000]

primes = primes_ candidates
primes_ (x:c) =
   x : primes_ (filter (\e -> e `mod` x /= 0) c)

main =
   putStrLn $ show $ sum primes


