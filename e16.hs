import Char

calcPow2 0 = 1
calcPow2 n = 2 * (calcPow2 (n-1))

main = do
    let a = calcPow2 1000
        b = map (\x -> (ord x) - (ord '0')) $ show a
        in putStrLn $ show $ sum b