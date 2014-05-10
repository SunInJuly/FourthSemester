data Polynome a = Polynome [(a, Int)]



instance (Num a, Ord a, Show a) => Show (Polynome a) where
    show (Polynome []) = show 0
    show (Polynome [x]) = show(fst x) ++ "*" ++ showDeg(snd x)
             where showDeg 0 = ""
                   showDeg 1 = "x"
                   showDeg d = "x^" ++ show d
    show (Polynome (x:xs)) = show(fst x) ++ "*" ++ showDeg(snd x) ++ " + " ++ show (Polynome xs)
             where showDeg 0 = ""
                   showDeg 1 = "x"
                   showDeg d = "x^" ++ show d

multiply' x y = ((fst x) * (fst y), (snd x) + (snd y))
multiply'' :: (Num a, Ord a, Show a) => [(a, Int)] -> (a, Int) -> [(a, Int)]
multiply'' xs val = helper xs val []
       where helper [] val new = new
             helper (x:xs) val new = helper xs val ((multiply' x val):new)

add' x y = ((fst x) + (fst y), snd y)
add'' :: (Num a, Ord a, Show a) => [(a, Int)] -> (a, Int) -> [(a, Int)]
add'' xs val = helper xs val [] False
        where helper xs val new True = xs ++ new
              helper [] val new False = val:new
              helper (x:xs) val new False | (snd x == snd val) = (helper xs val ((add' x val):new) True)
                                            | otherwise =  helper xs val (x:new) False
toList :: Polynome a -> [(a, Int)]
toList (Polynome pair) = pair

                   
simplify' new [] = new
simplify' new (x:xs) = simplify' (add'' new x) xs
simplify :: (Num a, Ord a, Show a) => Polynome a -> Polynome a
simplify (Polynome xs) = Polynome (simplify' [] (toList $ Polynome xs))

add :: (Num a, Ord a, Show a) => Polynome a -> Polynome a -> Polynome a 
add pol1 pol2 = simplify $ Polynome ((helper (toList pol1) (toList pol2)))
     where helper [] p2 = p2
           helper p1 [] = p1
           helper xs (y:ys) = helper (add'' xs y) ys
           
multiply :: (Num a, Ord a, Show a) => Polynome a -> Polynome a -> Polynome a 
multiply pol1 pol2 = simplify $ Polynome(helper (toList pol1) (toList pol2))
     where helper [] p2 = []
           helper p1 [] = []
           helper xs (y:ys) = toList $ add (Polynome (multiply'' xs y)) (Polynome (helper xs ys))                  

main = do
    let p = Polynome[(2,5),(1,1),(9,4)]
    putStr "p1 = "
    putStrLn(show $ simplify p)
    let p2 = Polynome[(3,4),(2,1)]
    putStr "p2 = "
    putStrLn(show $ simplify p2)
    putStr "p1 + p2 = "
    putStrLn(show $ add p p2)
    putStr "p1 * p2 = "
    putStrLn(show $ multiply p p2)