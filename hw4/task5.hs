--Написать программу, которая в диалоговом режиме позволяет осуществлять следующие операции:
--0 - exit
--1 - add value to sorted list
--2 - remove value from list
--3 - print list
--Все операции должны сохранять сортированность. Начинаем с пустого списка

import Data.List
import IO


loop :: [Int] -> IO ()
loop list = 
    print "0 - exit \n" >>
    print "1 - add value to the list \n" >>
    print "2 - remove value from the list \n" >>
    print "3 - print the list \n> " >>
   getLine >>= 
    \command -> case command of
        '0':_ -> print("Ok, quit")
        '1':_ -> do print ("Enter value: ")
                    value <- readLn
                    loop (insert value list)
        '2':_ -> do putStrLn ("Enter value: ")
                    value <- readLn
                    loop (delete value list)
        '3':_ -> do print(list)
                    loop list
        _     -> do putStrLn("Error. Try again.")
                    loop list
                    
main = loop []                    