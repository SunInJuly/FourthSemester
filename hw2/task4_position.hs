
position :: Int -> [Int] -> Int  
position _[] = 0
position n (x : xs) 
        | (x == n) = 0
        | otherwise = 1 + position n xs
main = print (position 1 [4, 2, 1, 3])
       