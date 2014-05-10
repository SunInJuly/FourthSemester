--Реализовать тип данных BinarySearchTree и соответствующие функции для работы с ним: добавление, удаление, поиск, размер (число элементов), высота

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)
------
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Node Empty x Empty
add x this @(Node left a right)
        | x == a = this
        | x <  a = (Node (add x left) a right)
        | x >  a = (Node left a (add x right))

remove :: (Ord a) => a -> Tree a -> Tree a
remove x Empty = Empty
remove x this @(Node left k right)
        | x == k = remove' this
        | x < k  = Node (remove x left) k right
        | x > k  = Node left k (remove x right)

remove' :: (Ord a) => Tree a -> Tree a
remove' (Node Empty _ right) = right
remove' (Node left _ Empty)  = left
remove' (Node left _ right)  = (Node left k' (remove k' right))
        where k' = minElem right

minElem :: (Ord a) => Tree a -> a
minElem (Node Empty k _) = k
minElem (Node left _ _)  = minElem left
    
find :: (Ord a) => a -> Tree a -> Bool
find x_ Empty = False
find x (Node left k right)
        | k == x = True
        | k > x  = find x left
        | k < x  = find x right

height :: (Num a, Ord a) => Tree a -> Int
height Empty        = -1
height (Node l _ r) = 1 + (max (height l) (height r))

size' :: (Ord a) => Tree a -> [a]
size' Empty        = []
size' (Node l k r) = size' l ++ [k] ++ size' r

size :: (Ord a) => Tree a -> Int
size tree = length $ size' tree


main = do 
let o = Empty
let a = add 12 (add 10 (add 8 (add 7 (add 1 o))))
print (height a)
print (size a)
print (minElem a)
print (find 1 a)
let b = remove 1 a 
print (find 1 b)

