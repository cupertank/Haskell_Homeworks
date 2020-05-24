myReverse' :: [Int] -> [Int] -> [Int]
myReverse' [] output = output
myReverse' (first:others) output = myReverse' others (first:output)

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse input = myReverse' input []
