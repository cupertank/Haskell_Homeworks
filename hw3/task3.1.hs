-- func x l = map (\y -> y * x) l        #
-- func x = map (\y -> y * x)            # eta редукция l
-- func x = map (*x)			 # Замена лямбды на частичную функцию
-- func x = (map . (*)) x		 # Композиция между map и (*), вынос x
-- func = map . (*)			 # eta редукция x

func = map . (*)
