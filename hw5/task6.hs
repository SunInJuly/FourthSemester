data Expression a = Const a |
                    Variable | 
                    Add (Expression a) (Expression a) |
                    Subtract (Expression a) (Expression a) |
                    Multiply (Expression a) (Expression a) |
                    Divide (Expression a) (Expression a) |
                    Pow (Expression a) Int

diff (Const _) = Const 0
diff Variable = Const 1
diff (Add x y) = Add (diff x) (diff y)
diff (Subtract x y) = Subtract (diff x) (diff y)
diff (Multiply x y) = Add (Multiply x (diff y)) (Multiply y (diff x))
diff (Divide x y) = Divide (Subtract (Multiply (diff x) y) (Multiply x (diff y))) (Pow y 2)
diff (Pow Variable 1) = Const 1
diff (Pow Variable n) = Multiply (Const n) (Pow Variable (n - 1))
diff (Pow x n) = Multiply (Const n) (Multiply (Pow x (n - 1)) (diff x))

--упрощение

simplify' (Add a (Const 0)) = a
simplify' (Add (Const 0) a) = a

simplify' (Subtract (Const 0) a) = Multiply (Const (-1)) a
simplify' (Subtract a (Const 0)) = a

simplify' (Multiply (Const 1) a) = a
simplify' (Multiply a (Const 1)) = a
simplify' (Multiply _ (Const 0)) = Const 0
simplify' (Multiply (Const 0) _) = Const 0
simplify' (Multiply (Const a) (Const b)) = Const (a*b)

simplify' (Divide a (Const 1)) = a
simplify'(Divide _ (Const 0)) = error "Divide by zero"
simplify' (Divide (Const 0) _) = Const 0

simplify' (Pow (Const 0) _) = Const 0 
simplify' (Pow _ 0) = Const 1
simplify' (Pow a 1) = a
simplify' (Pow (Const 1) _) = Const 1
simplify' a = a

simplify (Add x y) = simplify' (Add (simplify x) (simplify y))
simplify (Subtract x y) = simplify' (Subtract (simplify x) (simplify y))
simplify (Multiply x y) = simplify' (Multiply (simplify x) (simplify y))
simplify (Divide x y) = simplify' (Divide (simplify x) (simplify y))
simplify (Pow x n) = simplify' (Pow (simplify x) n)
simplify a = a


showBrackets expr = "(" ++ show expr ++ ")"

showMult (Add x y) = showBrackets (Add x y)
showMult (Subtract x y) = showBrackets (Subtract x y)
showMult (Divide x y) = showBrackets (Divide x y)
showMult x = show x

showDeg (Add x y) = showBrackets (Add x y)
showDeg (Subtract x y) = showBrackets (Subtract x y)
showDeg (Multiply x y) = showBrackets (Multiply x y)
showDeg (Divide x y) = showBrackets (Divide x y)
showDeg (Pow x y) = showBrackets (Pow x y)
showDeg x = show x

showDiv (Add x y) = showBrackets (Add x y)
showDiv (Subtract x y) = showBrackets (Subtract x y)
showDiv (Multiply x y) = showBrackets (Multiply x y)
showDiv (Divide x y) = showBrackets (Divide x y)
showDiv x = show x

showSubt (Add x y) = showBrackets (Add x y)
showSubt (Subtract x y) = showBrackets (Subtract x y)
showSubt x = show x

instance Show a => Show (Expression a) where
            show (Const a) = show a
            show Variable = "x"
            show (Add x y) = show x ++ " + " ++ show y
            show (Multiply x y) = showMult x ++ " * " ++ showMult y
            show (Pow x y) = showDeg x ++ " ^ " ++ show y
            show (Subtract x y) = show x ++ " - " ++ showSubt y
            show (Divide x y) = showDiv x ++ " / " ++ showDiv y

main = do
    let exp = Multiply (Const 10) Variable
    putStr ("(10x)' = ")
    putStrLn (show $ simplify (diff exp))

    let exp2 = (Add (Pow Variable 6) (Add (Multiply (Const 2) Variable) (Const 14))) 
    putStr ("(x^6 + 2x + 14)' = ")
    putStrLn (show $ simplify (diff exp2))