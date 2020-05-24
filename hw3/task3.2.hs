kek = kek where
	kek0 = 0:kek
	kek1 = concatMap (\x -> replicate 3 (x * 10)) kek0
	kek = zipWith (+) kek1 (cycle [1, 7, 9])
