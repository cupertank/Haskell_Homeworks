first :: [Int] -> Int
first x = sum (map (\i -> if even i then 1 else 0) x)

second :: [Int] -> Int
second x = length (filter even x)

third :: [Int] -> Int
third = foldr (\i acc -> if even i then acc + 1 else acc) 0 