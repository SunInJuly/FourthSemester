data Tree a = Nil
            | Node (Tree a) a (Tree a)
    deriving (Eq)
    
treeToString' str Nil = 'e':str
treeToString' str (Node leftSubtree x rightSubtree) = 'n':x:(treeToString' (treeToString' str leftSubtree) rightSubtree)
treeToString :: Tree Char -> String
treeToString = treeToString' ""


strToTree' ('e':xs) = (Nil, xs)
strToTree' ('n':x:xs) = let (leftSubtree, leftRemain) = strToTree' xs in
                                let (rightSubtree, rightRemain) = strToTree' leftRemain in
                                     (Node leftSubtree x rightSubtree, rightRemain)
strToTree :: String -> Tree Char
strToTree = fst . strToTree'

print' xs Nil = xs
print' xs (Node leftSubtree a rightSubtree) = print' ((print' xs leftSubtree) ++ [a]) rightSubtree
printTree :: Tree a -> [a]
printTree = print' []
            
main = do
    let tr = Node 
                    (Node
                         (Node 
                              Nil
                         '6' 
                              (Node
                                   Nil
                              '3'
                                   Nil))
                     '3'
                          (Node
                               Nil 
                          '8' 
                               (Node
                                   Nil 
                                '1' 
                                   (Node 
                                       Nil 
                                    '9'
                                        Nil))))
                 '0'
                    (Node 
                          Nil 
                    '0'
                          Nil)
    print (treeToString tr)
    let tr2 = strToTree "n0n0een3n8n1n9eeeen6n3eee"
    print(printTree tr2)