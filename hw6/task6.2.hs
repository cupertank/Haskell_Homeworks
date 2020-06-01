import System.Random
import Control.Monad.State

data BSTree a = Nil 
              | Node {val :: a, left :: BSTree a, right :: BSTree a}
                deriving Show

{--add :: (Ord a, Show a) => a -> State (BSTree a) (BSTree a)
add val = do
    tree <- get
    case tree of
        Nil -> do
            put (Node val Nil Nil)
            return (Node val Nil Nil)
        (Node y left right) -> case (val <= y) of
                True -> do
                    let (state, leftTree) = runState (add val) left
                    put (Node y leftTree right)
                    return state
                otherwise -> do
                    let (state, rightTree) = runState (add val) right
                    put (Node y left rightTree)
                    return state--}

add :: (Ord a) => a -> BSTree a -> BSTree a
add x Nil = Node x Nil Nil
add x (Node y left right)
                         | x <= y    = Node y (add x left) right
                         | otherwise = Node y left (add x right)

remove :: (Ord a) => a -> BSTree a -> BSTree a

remove _ Nil = Nil

remove x node
             | x < val node  = node { left = remove x (left node) }
             | x > val node  = node { right = remove x (right node)}
             | otherwise     = case node of
                               (Node _ Nil Nil) -> Nil        -- Leaf
                               (Node _ Nil _)   -> right node -- Only right subtree
                               (Node _ _ Nil)   -> left node  -- Only left subtree
                               otherwise        -> do
                                                   let maybeLeftVal = findLeft (right node) -- Самое левое значение в правом ребёнке
                                                   case maybeLeftVal of
                                                    (Just leftVal) -> Node leftVal (left node) (remove leftVal (right node))
                                                    Nothing        -> Nil -- Это на самом деле не должно произойти, благодаря 3 кейсу
-- Helper function for remove
findLeft :: (Ord a) => BSTree a -> Maybe a
findLeft Nil  = Nothing
findLeft node = case left node of
                Nil       -> Just (val node)
                otherwise -> findLeft (left node)

findByKey :: (Ord a) => a -> BSTree a -> Maybe (BSTree a)
findByKey _ Nil = Nothing
findByKey x node
                | x == val node = Just node
                | x < val node  = findByKey x (left node)
                | x > val node  = findByKey x (right node)

size :: BSTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

height :: BSTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

mapTree :: (a -> State state b) -> BSTree a -> State state (BSTree b)
mapTree fun Nil = return Nil
mapTree fun (Node y left right) = do
    y' <- fun y
    left' <- mapTree fun left
    right' <- mapTree fun right
    return (Node y' left' right')

randVals tree = do 
    gen <- newStdGen
    return (runState (mapTree randNode tree) gen)

randNode _ = do
    gen <- get
    let (val, gen') = randomR (1, 100) gen
    put gen'
    return val 

-- Test Add
test = (add 4 . add 7 . add 13 . add 1 . add 6 . add 14 . add 3 . add 10 . add 8) Nil -- Tree from BST page on Wikipedia

test1 = remove 3 test
test2 = remove 8 test1

test3 = size test2 == 7

test4 = height test2 == 4

test5 = findByKey 13 test2

numberTree tree = runState (mapTree number tree) 0

number ver = do
    cur <- get
    put (cur + 1)
    return (ver, cur)