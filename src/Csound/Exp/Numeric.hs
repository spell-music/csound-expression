{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Csound.Exp.Numeric where

import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons 
import Csound.Exp.NumExp

import qualified Csound.Exp.NumExp as NumExp

-------------------------------------------------------
-- instances for numerical expressions

instance NumOpt E where
    maybeInt x = case ratedExpExp $ unFix x of
        ExpPrim (PrimInt n) -> Just n
        _ -> Nothing
     
    maybeDouble x = case ratedExpExp $ unFix x of
        ExpPrim (PrimDouble d) -> Just d
        _ -> Nothing
    
    fromInt = prim . PrimInt
    fromDouble = prim . PrimDouble
    fromNum = noRate . ExpNum        

--------------------------------------------
-- numeric instances

instance Num E where    
    (+) = add 
    (*) = mul
    (-) = sub
    negate = neg
    fromInteger = fromInt . fromInteger
    abs = abs'
    signum = un

instance Fractional E where
    (/) = NumExp.div
    fromRational = fromDouble . fromRational    
    
instance Floating E where
    pi = fromDouble pi
    exp = exp'
    sqrt = sqrt'
    log = log'
    logBase a n = case n of
        2 -> logbtwo' a
        10 -> log10' a
        b -> log' a / log' b
    (**) = pow'
    sin = sin'
    tan = tan'
    cos = cos'
    asin = sininv'
    atan = taninv'
    acos = cosinv'
    sinh = sinh'
    tanh = tanh'
    cosh = cosh'
    asinh a = log $ a + sqrt (a * a + 1)
    acosh a = log $ a + sqrt (a + 1) * sqrt (a - 1)
    atanh a = 0.5 * log ((1 + a) / (1 - a))
{-    
instance Enum E where
    succ = (+1)
    pred = \x -> x - 1
    toEnum = fromInt
    enumFrom a = a : enumFrom (a+1)
    enumFromThen a b = a : enumFromThen (a+b) b
    enumFromTo = undefined
    enumFromThenTo = undefined
        
instance Real E where
    toRational = undefined
        
instance Integral E where
    quot a b = truncate $ (truncate a) / (truncate b)
    rem a b = (a `quot` b)*b - a
    mod = mod'
    div a b = truncate $ a - mod a b / b
    toInteger = undefined    
    
instance RealFrac E where
    properFraction a = (floor' a, frac' a)
    truncate = truncate'
    floor = floor'
    ceiling = ceil'
    round = round' . kr    
-}        
onE1 :: (Val a, Val b) => (E -> E) -> (a -> b)
onE1 f = wrap . unFix . f . Fix . unwrap

onE2 :: (Val a, Val b, Val c) => (E -> E -> E) -> (a -> b -> c)
onE2 f a b = wrap $ unFix $ f (Fix $ unwrap a) (Fix $ unwrap b)

fromE :: Val b => (a -> E) -> (a -> b)
fromE f = wrap . unFix . f 

-------------------------------------------
-- wrappers

instance Num Sig where    
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = fromE fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Num Int' where
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = fromE fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Num Double' where
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = fromE fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Fractional Sig where
    (/) = onE2 (/)
    fromRational = fromE fromRational

instance Fractional Double' where
    (/) = onE2 (/)
    fromRational = fromE fromRational

instance Floating Sig where
    pi = wrap $ unFix pi
    exp = onE1 exp
    sqrt = onE1 sqrt
    log = onE1 log
    logBase = onE2 logBase
    (**) = onE2 (**)
    sin = onE1 sin
    tan = onE1 tan
    cos = onE1 cos
    asin = onE1 asin
    atan = onE1 atan
    acos = onE1 acos
    sinh = onE1 sinh
    tanh = onE1 tanh
    cosh = onE1 cosh
    asinh = onE1 asinh
    acosh = onE1 acosh
    atanh = onE1 atanh
   
instance Floating Double' where
    pi = wrap $ unFix pi
    exp = onE1 exp
    sqrt = onE1 sqrt
    log = onE1 log
    logBase = onE2 logBase
    (**) = onE2 (**)
    sin = onE1 sin
    tan = onE1 tan
    cos = onE1 cos
    asin = onE1 asin
    atan = onE1 atan
    acos = onE1 acos
    sinh = onE1 sinh
    tanh = onE1 tanh
    cosh = onE1 cosh
    asinh = onE1 asinh
    acosh = onE1 acosh
    atanh = onE1 atanh




