-- С помощью оператора >>= опишите функцию, которая для данного числа n создает список из всех попарных произведений чисел от 1 до n. 
--( Т.е. что-то такое: [1*1, 1*2, 1*3, …, 1*n, 2*1, 2*2, …, n*n] - всего n*n элементов)
f4:: Int -> [Int]
f4 1 = [1]
f4 n = l >>=  \x1 -> l >>= 
              \x2 -> return (x1 * x2)
            where l = [1, 2 .. n]
main = print (f4 5)            