program n = [1..n] >>= (\x -> map (*x) [1..n])