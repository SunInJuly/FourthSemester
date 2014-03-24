import Data.List
-- 1 простые числа 
divisors:: Int -> [Int]
divisors x = [n | n <- [1..x-1], mod x n == 0]
isPrime:: Int -> Bool
isPrime x = (divisors x == [1])
primes = [n | n <- [2..], isPrime n]
f1 = print (take 5 primes )

--4 
f :: [Double] -> Double -> Double -> Double
f [] sum mul = sum / mul 
f  (x:xs) sum mul = f xs (sum + x) (mul * cos x) 

f4 :: [Double] -> Double
f4 x = f x 0 0
main = f4 [0, 1, 1]

-- 2 дерево

data BTree a = Leaf a | Branch (BTree a) a (BTree a)

f2 ::  BTree a ->[a] 
f2  tree = search tree []
search (Leaf a) xs = if a < 0 then a:xs else xs 
search (Branch lb node rb) xs = if node < 0 
                                then node : xs' 
                                else xs' 
                                where xs' = search lb (search rb xs)


                                
                                
          