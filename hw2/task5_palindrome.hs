palindrome :: (Eq a) => [a] -> Bool 
palindrome xs = xs == reverse xs

main = print (palindrome [1, 2, 3, 2, 1])
