import Data.List

data Member = Member { degree :: Int,  index :: Int}

type Polynom = [Member]

instance Show Member where
    show (Member 0 index) = show index
    show (Member 1 index) = show index ++ "*x"
    show mem = show (index mem) ++ "*x^"  ++ show (degree mem)

    showList [] = (++ "")
    showList (x:[]) = (++ show x)
    showList (x:xs) = (++ show x ++ "+" ++ show xs)


add :: Polynom -> Polynom -> Polynom
add first second = optimizator (first ++ second)

multiply :: Polynom -> Polynom -> Polynom
multiply first second = optimizator [Member (degree x + degree y) (index x * index y) | x <- first, y <- second]

optimizator :: Polynom -> Polynom
optimizator polynom = do
    let grouped = groupBy (\left right -> degree left == degree right) polynom :: [Polynom]
    let summed = map (foldr1 (\first second -> Member (degree first) (index second + index second))) grouped
    let filtered = filter (\element -> index element > 0) summed
    filtered

