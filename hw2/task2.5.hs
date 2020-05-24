first :: [Int] -> Int
first [] = 0
first (x:xs) = x

other :: [Int] -> [Int]
other [] = []
other (x:xs) = xs

zipper' :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]

zipper' [] [] [] ans = ans
zipper' x y z ans = zipper' (other x) (other y) (other z) (((first x) + (first y) + (first z)) : ans)

zipper x y z = zipper' x y z []
