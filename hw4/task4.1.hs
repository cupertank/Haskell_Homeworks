first :: [Int] -> Int
first = sum . map (\i -> if even i then 1 else 0)

second :: [Int] -> Int
second = length . filter even

third :: [Int] -> Int
third = foldr (\i acc -> if even i then acc + 1 else acc) 0 