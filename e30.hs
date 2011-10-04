import Char

isPow5Sum a =
    let p5sum = sum $ map (^5) $ map (\x -> (ord x) - (ord '0')) $ show a
        in a == p5sum

-- Maxiumum number that can be written as the sum of fith powers is
-- n*9^5 and it must be possible to write the number 10^(n-1) with it,
-- so we can stop the search after the number 10^n - 1 where n is the
-- largest integer that satisfies n*9^5 >= 10^(n-1) => n = 6.
main = putStrLn $ show $ sum $ filter isPow5Sum [10..(10^7-1)]