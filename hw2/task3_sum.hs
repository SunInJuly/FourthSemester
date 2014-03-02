digits :: Int -> Int
digits 0 = 0
digits x = (x `mod` 10) + digits (x `div` 10)

main = print (digits 123)