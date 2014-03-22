-- cписок чисел из 1 7 9
next:: Int -> Int 
next 1 = 7 
next 7 = 9
next 9 = 11
next x  
        | md == 9        = (next dv) * 10 + 1
        | otherwise      = dv * 10 + next md
   where
        md = mod x 10 
        dv = div x 10         
list = 1 : map next list 

main = print ( take 10 list ) 
        
