--сгенерировать список из  1-1 и на его основе 1 -2 3 -4 ...  
import Data.List
import System.Random


f1 :: [Int]
f1  = scanl1 (*) (1:[-1, -1..])
f2:: [Int]
f2  = zipWith (*) f1 [1, 2..]


-- получает список возвращает такой же список рандомных чисел 

f3 ::RandomGen g => g -> [Int] -> [Int]
f3 g xs = (take (length xs) (randoms g :: [Int]))

main = do
 g <- getStdGen
 print (f3 g [1, 2, 3])

