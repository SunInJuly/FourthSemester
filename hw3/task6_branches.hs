-- проверка скобочной последовательности 
checkPair :: Char -> Char -> Bool
checkPair '('')' = True 
checkPair '['']' = True 
checkPair '{''}' = True
checkPair _ _ = False 

checkBrackets :: [Char] -> [Char] -> Bool
checkBrackets [] [] = True
checkBrackets [] _ = False 
checkBrackets (x:xs) [] = checkBrackets xs (x : [])
checkBrackets (x:xs) stack | checkPair (head stack) x   = checkBrackets xs (tail stack) 
                           | otherwise                  = checkBrackets xs (x : stack) 
                           
                
checkString :: [Char] -> Bool
checkString x = checkBrackets(filter (`elem` "() [] {}") x) []

main = do 
        print (checkString "[(as(asdf)asdfsdf)]")
        print (checkString "[")
        print (checkString "{{))")
        
 