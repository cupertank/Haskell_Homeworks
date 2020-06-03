genList = gen where
	  gen0 = 0:gen
	  gen1 = concatMap (\x -> replicate 3 (x * 10)) gen0
	  gen = zipWith (+) gen1 (cycle [1, 7, 9])
