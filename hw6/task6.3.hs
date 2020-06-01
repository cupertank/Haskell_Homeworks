import Control.Monad.State
import Data.List

data Graph v = Graph [(Int, v)] [(Int, Int, Int)] deriving Show

data CycleState = CycleState [Len] [Bool] [Int] deriving Show -- (Кратчайшие пути) (Флаги) (Родители)

data Len = INF
         | Len Int
         deriving Show

instance Eq Len where
    INF == INF         = True
    (Len x) == (Len y) = x == y
    _ == _             = False
    x /= y             = not (x == y)

instance Ord Len where
     compare (Len x) INF     = LT
     compare (Len x) (Len y) = compare x y
     compare INF (Len x)     = GT
     compare INF INF         = EQ
     
     INF <= INF              = True
     INF <= _                = False
     _ <= INF                = True
     (Len x) <= (Len y)      = x <= y

     INF < INF               = False
     INF < _                 = False
     _ < INF                 = True
     (Len x) < (Len y)       = x < y

initState :: Int -> Int -> CycleState
initState n startNode = CycleState [x | i <- [1..n], let x = if i == startNode then (Len 0) else INF] [False | _ <- [1..n]] [-1 | _ <- [1..n]]

replaceByIndex _ _ [] = []
replaceByIndex n val (x:xs)
    | n == 0    = val:xs
    | otherwise = x:replaceByIndex (n-1) val xs

findMin :: [Len] -> [Bool] -> Maybe (Int, Len)
findMin lens flags = (findMin' lens flags 1 Nothing)
    where
        findMin' :: [Len] -> [Bool] -> Int -> Maybe (Int, Len) -> Maybe (Int, Len)
        findMin' (len:xs) (flag:ys) i Nothing = if not flag then findMin' xs ys (i+1) (Just (i, len)) else findMin' xs ys (i+1) Nothing
        
        findMin' (len:xs) (flag:ys) i (Just (minInd, minLen)) = if len < minLen && not flag then findMin' xs ys (i+1) (Just (i, len)) 
                                                                                                else findMin' xs ys (i+1) (Just (minInd, minLen))
        findMin' _ _ _ ans = ans

lensUpdater _ [] lens parents = (lens, parents)
lensUpdater (Len startLen) ((from, to, weight):xs) lens parents = 
    if Len (startLen + weight) < (lens !! (to - 1) ) 
        then lensUpdater (Len startLen) xs (replaceByIndex (to-1) (Len (startLen + weight)) lens) (replaceByIndex (to-1) from parents)
        else lensUpdater (Len startLen) xs lens parents

dijkstraHelper :: Graph v -> State CycleState ([Len], [Int])
dijkstraHelper g@(Graph nodes edges) = do
    (CycleState lens flags parents) <- get
    let maybeMinNode = findMin lens flags
    case maybeMinNode of
        Nothing -> return (lens, parents)   
        Just (node, len) -> do 
            let newFlags = replaceByIndex (node-1) True flags
            let edgesFromNode = (filter (\(from, _, _) -> from == node) edges) 
                                `union` ((map (\(from, to, weight) -> (to, from, weight)) . (filter (\(_, to, _) -> to == node))) edges) -- Надстройка если неориентированный граф
            let (newLens, newParents) = lensUpdater len edgesFromNode lens parents
            put (CycleState newLens newFlags newParents)
            dijkstraHelper g


--dijkstra :: Graph v -> Int -> [Int]
dijkstra g@(Graph nodes edges) startNode = do
    let n = length nodes
    let (lens, parents) = evalState (dijkstraHelper g) (initState n startNode)
    let paths = findPaths startNode parents n
    (lens, paths)

findPaths startNode parents n = [findPath' startNode x mzero | x <- [1..n]] :: [[Int]]
    where
        findPath' :: Int -> Int -> [Int] -> [Int] 
        findPath' start end path
            | start == end = return end `mplus` path
            | otherwise    = findPath' start (parents !! (end-1)) (return end `mplus` path)

graph1 = Graph [(1, 'e'), (2, 'f'), (3, 'g'), (4, 'h'), (5, 'j')] [(1, 3, 8), (1, 4, 12), (1, 5, 14), (2, 3, 16), (2, 5, 10), (3, 4, 6)]
graph2 = Graph [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')] [(1, 2, 12), (1, 3, 4), (3, 4, 2), (2, 3, 20)]

test1 = dijkstra graph1 1
-- Lengths = [Len 0,Len 24,Len 8,Len 12,Len 14]
-- Paths   = [[1], [1, 3, 2], [1, 3], [1, 4], [1, 5]]

test2 = dijkstra graph1 2

-- Lengtgs = [Len 24,Len 0,Len 16,Len 22,Len 10]
-- Paths   = [[2,5,1],[2],[2,3],[2,3,4],[2,5]]

test3 = dijkstra graph2 2

-- Lengths = [Len 12,Len 0,Len 16,Len 18]
-- Paths   =[[2,1],[2],[2,1,3],[2,1,3,4]]