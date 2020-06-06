fib n
	| n >= 0 = fibHelper n 0 1
	| n < 0  = fibHelper (abs n) 0 1 * (-1) ^ ((abs n) + 1)

fibHelper 0 acc _ = acc
fibHelper n acc prev = fibHelper (n - 1) (acc + prev) acc
