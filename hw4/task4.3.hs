data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldr f acc Empty = acc
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Node l k r) = foldr f (f k (foldr f acc r)) l

program :: Tree a -> [a]
program tree = foldr (:) [] tree

-- Example
test = Node (Node (Leaf 1) 2 Empty) 3 (Node (Leaf 4) 5 (Leaf 6))
ans = program test

