-- первая позиция, где сумма соседних элементов максимальна
import Data.Maybe
import Data.List

sumlist :: [Int] -> [Int]
sumlist xs = zipwith (+) xs (0:xs)

maxposition :: [Int] -> Int 
maxposition[] = -1
maxposition[_] = -1
maxposition = fromJust $ elemIndex (maximum $ sumlist xs) (sumlist xs)
main = print (maxposition[1, 5, 6, 2])