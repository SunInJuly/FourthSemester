-- первая позиция, где сумма соседних элементов максимальна
import Data.Maybe
import Data.List

sumlist :: [Int] -> [Int]
sumlist xs = zipWith (+) xs (0:xs)

maxposition :: [Int] -> Int 
maxposition [] = 0
maxposition [_] = 0
maxposition xs = fromJust $ elemIndex (maximum $ sumlist xs) (sumlist xs)
main = print (maxposition[1, 5, 6, 2])