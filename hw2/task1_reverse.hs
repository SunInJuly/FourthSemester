reverse0 :: [a] -> [a] -> [a]
reverse0 xs [] = xs 
reverse0 xs (y:ys) = reverse0(y:xs) ys

myReverse :: [a] -> [a]
myReverse xs = reverse0 [] xs

main = print(myReverse [1, 2, 3])
