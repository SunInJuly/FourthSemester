
--Используя монадичесие функции, опишите фунцию, которая ищет в списке первый элемент, больший своих соседей (предыдущего и следующего)
import Control.Monad

biggest :: [Int] -> Maybe Int
biggest [] = Nothing
biggest (x:[]) = Nothing
biggest (x:y:[]) = Nothing
biggest (x:y:z:xs) = return xs >>= (\ys -> 
                                if (x < y) && (y > z) then Just y 
                                else biggest (y:z:ys))
main = do
        print( biggest ([1, 2, 3, 4, 5, 6, 7]))                                        
        print(biggest ([1, 2, 1]))                 