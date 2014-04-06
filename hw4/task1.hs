--Реализовать функцию поиска по некоторому условию в двоичном дереве. Условие передается в качестве параметра.

data BTree a = Leaf a | Branch (BTree a) a (BTree a)
search :: (a -> Bool) -> BTree a -> [a] -> [a]
search condition (Leaf a) xs = if condition a then a:xs else xs 
search condition (Branch lb node rb) xs = if condition node
                                then node : xs' 
                                else xs' 
                                where xs' = search condition lb (search condition rb xs)
search_condition ::  (a -> Bool) -> BTree a -> [a]
search_condition condition tree = search condition tree []
