--Реализовать три варианта функции, подсчитывающей количество четных чисел в списке 
--(с использованием стандартных функций map, filter, foldr). Использование рекурсии не допускается.
f1:: [Int] -> Int 
f1 xs = length( filter (==0) (map (`mod` 2) xs))

f2::[Int] -> Int
f2 = foldr (\x cnt -> if (x `mod` 2 == 0) then cnt + 1 else cnt) 0

f3 :: [Int] -> Int
f3 xs = length (filter even xs)

main = print (f2 [1, 2, 3, 4])

