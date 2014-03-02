degree :: Int -> [Int] -> [Int]
degree 0 xs = 1:xs
degree n xs = degree (n-1) (2^n : xs)

degrees :: Int -> [Int] 
degrees n = degree n []

main = print (degrees 3)
