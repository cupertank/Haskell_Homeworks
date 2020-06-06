finder' [] y cnt = cnt + 1
finder' (x:xs) y cnt = if (x == y) then cnt + 1 else finder' xs y (cnt + 1)

finder mas y = finder' mas y 0
