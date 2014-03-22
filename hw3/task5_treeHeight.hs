-- минимальная высота дерева и высота 
data BTree a = Leaf a | Branch (BTree a) (BTree a)
        
minHeight:: BTree a -> Int 
minHeight (Leaf a) = 0 
minHeight (Branch branchLeft branchRight) = 1 + min (minHeight branchLeft) (minHeight branchRight) 

maxHeight :: BTree a -> Int 
maxHeight (Leaf a) = 0 
maxHeight (Branch branchLeft branchRight) = 1 + max (maxHeight branchLeft) (maxHeight branchRight) 

