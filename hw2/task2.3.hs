numSum' 0 y = y
numSum' x y = numSum' (div x 10) y + (mod x 10)

numSum x = numSum' x 0

