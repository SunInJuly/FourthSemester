--Проверить, что все элементы списка удовлетворяют некоторому условию (условие передается как параметр)
import Data.List
f2:: (a -> Bool) -> [a] -> Bool
f2 condition ls = (length ls) == (length (filter (condition ls)))

main = print (f2 ((>0) [1, 2, 3]))