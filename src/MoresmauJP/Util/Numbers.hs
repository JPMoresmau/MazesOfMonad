-- | Numeric utilities
-- (c) JP Moresmau 2009
module MoresmauJP.Util.Numbers where

avg :: Integral a => [a] -> a
avg [] = error "Cannot take average of an empty list"
avg l=avg' (0,0) l
 	where 
 		avg' (a,b) []=div a b
		avg' (a,b) (x:xs)=avg' (a+x,b+1) xs

bindInt :: (Int,Int) -> Int -> Int
bindInt (low,high) score=max low (min high score)
