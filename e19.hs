data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

numToDay = zip [1..] [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

dayOfWeek :: Int -> Day
dayOfWeek daysSinceStart =
    let d = lookup (daysSinceStart `mod` 7 + 1) numToDay
        in case d of
            Just x -> x
            otherwise -> Monday

firstDayOfYear n =
    let n1 = n-1
        in 1 + 365 * n1 + n1 `div` 4 - n1 `div` 100 + n1 `div` 400

mothStartDif n = [0, 31, febStart, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where febStart = if n `mod` 4 /= 0 then 28 else 29
    
monthStart n =
    map (+firstDayOfYear n) $ map (\(n, l) -> sum $ take n $ l) $ zip [1..12] (repeat $ mothStartDif n)

main = putStrLn $ show $ length $ filter (== Sunday) $ map (dayOfWeek) $ foldl1 (++) $ map (monthStart) [1..100]