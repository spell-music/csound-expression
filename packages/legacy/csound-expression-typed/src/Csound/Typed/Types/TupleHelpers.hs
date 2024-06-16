module Csound.Typed.Types.TupleHelpers where

cons3 :: (a, (b, c)) -> (a, b, c)
cons4 :: (a, (b, c, d)) -> (a, b, c, d)
cons5 :: (a, (b, c, d, e)) -> (a, b, c, d, e)
cons6 :: (a, (b, c, d, e, f)) -> (a, b, c, d, e, f)
cons7 :: (a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)
cons8 :: (a, (b, c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h)

cons3 (a, (b, c)) = (a, b, c)
cons4 (a, (b, c, d)) = (a, b, c, d)
cons5 (a, (b, c, d, e)) = (a, b, c, d, e)
cons6 (a, (b, c, d, e, f)) = (a, b, c, d, e, f)
cons7 (a, (b, c, d, e, f, g)) = (a, b, c, d, e, f, g)
cons8 (a, (b, c, d, e, f, g, h)) = (a, b, c, d, e, f, g, h)

split3 :: (a, b, c) -> (a, (b, c))  
split4 :: (a, b, c, d) -> (a, (b, c, d))  
split5 :: (a, b, c, d, e) -> (a, (b, c, d, e))  
split6 :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))  
split7 :: (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))  
split8 :: (a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h))  

split3 (a, b, c) = (a, (b, c))  
split4 (a, b, c, d) = (a, (b, c, d))  
split5 (a, b, c, d, e) = (a, (b, c, d, e))  
split6 (a, b, c, d, e, f) = (a, (b, c, d, e, f))  
split7 (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))  
split8 (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))  

