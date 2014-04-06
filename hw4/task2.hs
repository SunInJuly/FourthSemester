--функция свертки для бинарного дерева
data BTree a = Leaf a
             | Branch (BTree a) a (BTree a)
             deriving (Eq, Show)
tree_to_list' :: [a] -> BTree a -> [a]
tree_to_list' xs (Leaf a)  =  a:xs
tree_to_list' xs (Branch left a right) = tree_to_list' (a:(tree_to_list' xs left)) right

tree_to_list :: BTree a -> [a]
tree_to_list tree = tree_to_list' [] tree

            
treeFoldl:: (a -> b -> a) -> a -> BTree b -> a
treeFoldl func x tree = foldl func x (tree_to_list tree)


main = do 
let n = Branch (Leaf 2) 6 (Branch (Leaf 1) 5 (Leaf 2))
let x = treeFoldl (+) 0 n 
show x
