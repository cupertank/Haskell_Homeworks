decompose :: Int -> [[Int]]
decompose n = concatMap (\i -> decomposeHelper n i) [1..n]

decomposeHelper :: Int -> Int -> [[Int]]
decomposeHelper n d = if n == d then [[n]] else map (\mas -> d:mas) (concatMap (\i -> decomposeHelper (n-d) i) [d..(n-d)])