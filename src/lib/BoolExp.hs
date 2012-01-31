module BoolExp where

data BoolExp a = Cond a
               | And (BoolExp a) (BoolExp a)
               | Or  (BoolExp a) (BoolExp a)
               | Not (BoolExp a)

check :: (a -> Bool) -> BoolExp a -> Bool
check f (Cond x)  = f x
check f (And x y) = check f x && check f y
check f (Or x y)  = check f x || check f y
check f (Not x)   = not (check f x)
