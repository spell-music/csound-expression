{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Csound.Exp.Numeric() where

import Data.Maybe(fromJust)
import Control.Applicative
import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons 

-------------------------------------------------------
-- instances for numerical expressions

class NumOpt a where
    maybeInt    :: a -> Maybe Int
    maybeDouble :: a -> Maybe Double
    fromInt     :: Int -> a
    fromDouble  :: Double -> a
    fromNum     :: NumExp a -> a

instance NumOpt E where
    maybeInt x = case ratedExpExp $ unFix x of
        ExpPrim (PrimInt n) -> Just n
        _ -> Nothing
     
    maybeDouble x = case ratedExpExp $ unFix x of
        ExpPrim (PrimDouble d) -> Just d
        _ -> Nothing
    
    fromInt = prim . PrimInt
    fromDouble = prim . PrimDouble
    fromNum = noRate . ExpNum . fmap toPrimOr   

--------------------------------------------
-- numeric instances

instance Num E where    
    (+) a b 
        | isZero a = b
        | isZero b = a
        | otherwise = biOpt (+) (+) Add a b
        
    (*) a b 
        | isZero a || isZero b = fromInt 0
        | otherwise = biOpt (*) (*) Mul a b
        
    (-) a b  
        | isZero a = negate b
        | isZero b = a
        | otherwise = biOpt (-) (-) Sub a b    
    
    negate = unOpt negate negate Neg
    
    fromInteger = fromInt . fromInteger
    abs = unOpt abs abs Abs
    signum = undefined

instance Fractional E where
    (/) a b 
        | isZero a = fromInt 0
        | isZero b = error "csound (/): division by zero" 
        | otherwise = biOptOnDouble (/) Div a b

    fromRational = fromDouble . fromRational    

instance Floating E where
    pi = fromDouble pi
    exp = funOpt exp ExpOp
    sqrt = funOpt sqrt Sqrt
    log = funOpt log Log
    logBase a n = case n of
        2 -> funOpt (flip logBase 2) Logbtwo a
        10 -> funOpt (flip logBase 10) Log10 a
        b -> log a / log b
    (**) = biOpt (^) (**) Pow
    sin = funOpt sin Sin 
    tan = funOpt tan Tan
    cos = funOpt cos Cos
    asin = funOpt asin Sininv
    atan = funOpt atan Taninv
    acos = funOpt acos Cosinv
    sinh = funOpt sinh Sinh
    tanh = funOpt tanh Tanh
    cosh = funOpt cosh Cosh
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

onConst :: Val b => (a -> E) -> (a -> b)
onConst f = wrap . unFix . f 

-------------------------------------------
-- wrappers

instance Num Sig where    
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = onConst fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Num I where
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = onConst fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Num D where
    (+) = onE2 (+)
    (*) = onE2 (*)
    (-) = onE2 (-)
    negate = onE1 negate
    fromInteger = onConst fromInteger
    abs = onE1 abs
    signum = onE1 signum

instance Fractional Sig where
    (/) = onE2 (/)
    fromRational = onConst fromRational

instance Fractional D where
    (/) = onE2 (/)
    fromRational = onConst fromRational

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
   
instance Floating D where
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

------------------------------------------------------------

isZero :: NumOpt a => a -> Bool
isZero a = maybe False id $ liftA2 (||) ((== 0) <$> maybeInt a) ((==0) <$> maybeDouble a)

getDouble :: NumOpt a => a -> Maybe Double
getDouble a = maybe (fromIntegral <$> maybeInt a) Just $ maybeDouble a 

unOpt :: (NumOpt a) => (Int -> Int) -> (Double -> Double) -> NumOp -> a -> a
unOpt intOp doubleOp op a = fromJust $
        (fromInt . intOp <$> maybeInt a)
    <|> (fromDouble . doubleOp <$> maybeDouble a)
    <|> Just (noOpt1 op a)

biOpt :: (NumOpt a) => (Int -> Int -> Int) -> (Double -> Double -> Double) -> NumOp -> a -> a -> a
biOpt intOp doubleOp op a b = fromJust $
        intOpA (maybeInt a) (maybeInt b) 
    <|> doubleOpA (getDouble a) (getDouble b)
    <|> Just (noOpt2 op a b)
    where intOpA a b = fromInt <$> liftA2 intOp a b
          doubleOpA a b = fromDouble <$> liftA2 doubleOp a b

biOptOnDouble :: (NumOpt a) => (Double -> Double -> Double) -> NumOp -> a -> a -> a
biOptOnDouble doubleOp op a b = fromJust $
        (fromDouble <$> liftA2 doubleOp (getDouble a) (getDouble b))
    <|> Just (noOpt2 op a b) 
        

funOpt :: NumOpt a => (Double -> Double) -> NumOp -> a -> a
funOpt doubleOp op a = fromJust $
        (fromDouble . doubleOp <$> getDouble a)
    <|> Just (noOpt1 op a)

noOpt1 :: NumOpt a => NumOp -> a -> a
noOpt1 op a = fromNum $ PreInline op [a]

noOpt2 :: NumOpt a => NumOp -> a -> a -> a
noOpt2 op a b = fromNum $ PreInline op [a, b]

doubleToInt :: NumOpt a => (Double -> Int) -> NumOp -> a -> a
doubleToInt fun op a = fromJust $
        (fromInt <$> maybeInt a)
    <|> (fromInt . fun <$> maybeDouble a)
    <|> Just (noOpt1 op a)

-- arithmetic

mod' :: NumOpt a => a -> a -> a
mod' = biOpt mod (\a b -> fromIntegral $ mod (floor a) (floor b)) Pow
 
-- other functions

ceil', floor', frac', int', round' :: NumOpt a => a -> a

ceil'   = doubleToInt ceiling Ceil 
floor'  = doubleToInt floor Floor
round'  = doubleToInt round Round
frac'   = unOpt (const 0) (snd . properFraction) Frac 
int'    = doubleToInt truncate IntOp 
    



    





