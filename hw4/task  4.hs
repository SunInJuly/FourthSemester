import Data.List 
checkUnequal :: (Eq a) => [a] -> Bool
checkUnequal = check' []
         where check' _ [] = True
               check' list (l:ls) | (elem l list) = False
                                   | otherwise = check' (l:list) ls

main = do 
print (checkUnequal [1, 2, 3, 4])
print (checkUnequal[1, 2, 1])