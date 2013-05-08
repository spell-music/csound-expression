{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Csound.Exp.Numeric(
    fracD, floorD, ceilD, intD, roundD,
    fracSig, floorSig, ceilSig, intSig, roundSig
) where

import Csound.Exp
import Csound.Exp.Wrapper(
    Sig, D, prim, double, noRate,
    Val(..), toExp, onE1, onE2)

--------------------------------------------
-- numeric instances

instance Num E where    
    (+) a b 
        | isZero a = b
        | isZero b = a
        | otherwise = biOpt (+) Add a b
        
    (*) a b 
        | isZero a || isZero b = fromDouble 0
        | otherwise = biOpt (*) Mul a b
        
    (-) a b  
        | isZero a = negate b
        | isZero b = a
        | otherwise = biOpt (-) Sub a b    
    
    negate = unOpt negate Neg
    
    fromInteger = fromDouble . fromInteger
    abs = unOpt abs Abs
    signum = undefined

instance Fractional E where
    (/) a b 
        | isZero a = fromDouble 0
        | isZero b = error "csound (/): division by zero" 
        | otherwise = biOpt (/) Div a b

    fromRational = fromDouble . fromRational    

instance Floating E where
    pi = fromDouble pi
    exp = unOpt exp ExpOp
    sqrt = unOpt sqrt Sqrt
    log = unOpt log Log
    logBase a n = case n of
        2 -> unOpt (flip logBase 2) Logbtwo a
        10 -> unOpt (flip logBase 10) Log10 a
        b -> log a / log b
    (**) = biOpt (**) Pow
    sin = unOpt sin Sin 
    tan = unOpt tan Tan
    cos = unOpt cos Cos
    asin = unOpt asin Sininv
    atan = unOpt atan Taninv
    acos = unOpt acos Cosinv
    sinh = unOpt sinh Sinh
    tanh = unOpt tanh Tanh
    cosh = unOpt cosh Cosh
    asinh a = log $ a + sqrt (a * a + 1)
    acosh a = log $ a + sqrt (a + 1) * sqrt (a - 1)
    atanh a = 0.5 * log ((1 + a) / (1 - a))

enumError name = error $ name ++ " -- is defined only for literals"
    
instance Enum E where
    succ = (+1)
    pred = \x -> x - 1
    toEnum = fromDouble . fromIntegral
    fromEnum = undefined
    enumFrom a = a : enumFrom (a+1)
    
    enumFromThen a b = a : enumFromThen (a + b) b
     
    enumFromTo a b = case (toNumOpt a, toNumOpt b) of
        (Left x, Left y) -> fmap fromDouble $ enumFromTo x y
        _ -> enumError "[a .. b]"
            
    enumFromThenTo a b c = case (toNumOpt a, toNumOpt b, toNumOpt c) of
        (Left x, Left y, Left z) -> fmap fromDouble $ enumFromThenTo x y z
        _ -> enumError "[a, b .. c]"
    
    
instance Real E where toRational = undefined
        
instance Integral E where
    quot a b = intE $ (intE a) / (intE b)
    rem a b = (a `quot` b) * b - a
    mod = mod'
    div a b = intE $ a - mod a b / b
    quotRem a b = (quot a b, rem a b)
    divMod a b = (div a b, mod a b)
    toInteger = undefined    

onConst :: Val b => (a -> E) -> (a -> b)
onConst f = fromE . f 

-------------------------------------------
-- wrappers

instance Real Sig where  toRational = undefined
instance Ord  Sig where  compare    = undefined
instance Eq   Sig where  (==)       = undefined
instance Real D   where  toRational = undefined
instance Ord  D   where  compare    = undefined
instance Eq   D   where  (==)       = undefined

instance Enum Sig where
    succ = onE1 succ
    pred = onE1 pred
    toEnum = fromE . toEnum
    fromEnum = fromEnum . toE
    enumFrom a = fmap fromE $ enumFrom $ toE a
    enumFromThen a b = fmap fromE $ enumFromThen (toE a) (toE b)
    enumFromTo   a b = fmap fromE $ enumFromTo   (toE a) (toE b)
    enumFromThenTo a b c = fmap fromE $ enumFromThenTo (toE a) (toE b) (toE c)     

instance Enum D where
    succ = onE1 succ
    pred = onE1 pred
    toEnum = fromE . toEnum
    fromEnum = fromEnum . toE
    enumFrom a = fmap fromE $ enumFrom $ toE a
    enumFromThen a b = fmap fromE $ enumFromThen (toE a) (toE b)
    enumFromTo   a b = fmap fromE $ enumFromTo   (toE a) (toE b)
    enumFromThenTo a b c = fmap fromE $ enumFromThenTo (toE a) (toE b) (toE c)  

instance Integral Sig where
    quot = onE2 quot
    rem  = onE2 rem
    div  = onE2 div
    mod  = onE2 mod
    quotRem a b = (fromE x, fromE y)        
        where (x, y) = quotRem (toE a) (toE b)
    divMod a b = (fromE x, fromE y)
        where (x, y) = divMod (toE a) (toE b)
    toInteger = toInteger . toE
        
instance Integral D where
    quot = onE2 quot
    rem  = onE2 rem
    div  = onE2 div
    mod  = onE2 mod
    quotRem a b = (fromE x, fromE y)        
        where (x, y) = quotRem (toE a) (toE b) 
    divMod a b = (fromE x, fromE y)
        where (x, y) = divMod (toE a) (toE b)
    toInteger = toInteger . toE

-- | Fractional part of the signal.
fracSig :: Sig -> Sig
fracSig = onE1 fracE

-- | Floor operator for signals.
floorSig :: Sig -> Sig 
floorSig = onE1 floorE

-- | Ceiling operator for signals.
ceilSig :: Sig -> Sig
ceilSig = onE1 ceilE

-- | Integer part of the number for signals.
intSig :: Sig -> Sig 
intSig = onE1 intE

-- | Round operator for signals.
roundSig :: Sig -> Sig
roundSig = onE1 roundE

-- | Fractional part of the number.
fracD :: D -> D
fracD = onE1 fracE

-- | Floor operator for numbers.
floorD :: D -> D 
floorD = onE1 floorE

-- | Ceiling operator for numbers.
ceilD :: D -> D
ceilD = onE1 ceilE

-- | Integer part of the number.
intD :: D -> D 
intD = onE1 intE

-- | Round operator for numbers.
roundD :: D -> D
roundD = onE1 roundE


instance Num Sig where    
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
    pi = fromE pi
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
    pi = fromE pi
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
-- Optimizations for constants
--
-- If an arithmetic expression contains constants we can execute
-- it and render as constant. We check wether all arguments 
-- are constants. If it's so we apply some numeric function and
-- propogate a constant value.

toNumOpt :: E -> Either Double E
toNumOpt x = case toExp x of
    ExpPrim (PrimDouble d) -> Left d
    _ -> Right x

fromNumOpt :: Either Double E -> E
fromNumOpt = either (prim . PrimDouble) id 

expNum :: NumExp E -> E
expNum = noRate . ExpNum . fmap toPrimOr

fromDouble = fromNumOpt . Left

isZero :: E -> Bool
isZero a = either ( == 0) (const False) $ toNumOpt a

-- optimization for unary functions
unOpt :: (Double -> Double) -> NumOp -> E -> E
unOpt doubleOp op a = fromNumOpt $ either (Left . doubleOp) (Right . noOpt1) $ toNumOpt a
    where noOpt1 a = expNum $ PreInline op [a] 

-- optimization for binary functions
biOpt :: (Double -> Double -> Double) -> NumOp -> E -> E -> E
biOpt doubleOp op a b = fromNumOpt $ case (toNumOpt a, toNumOpt b) of
    (Left da, Left db) -> Left $ doubleOp da db
    _ -> Right $ noOpt2 a b
    where noOpt2 a b = expNum $ PreInline op [a, b]

doubleToInt :: (Double -> Int) -> NumOp -> E -> E
doubleToInt fun = unOpt (fromIntegral . fun) 

-- arithmetic

mod' :: E -> E -> E
mod' = biOpt (\a b -> fromIntegral $ mod (floor a) (floor b)) Pow
 
-- other functions

ceilE, floorE, fracE, intE, roundE :: E -> E

ceilE   = doubleToInt ceiling Ceil 
floorE  = doubleToInt floor Floor
roundE  = doubleToInt round Round
fracE   = unOpt (snd . properFraction) Frac 
intE    = doubleToInt truncate IntOp 
    
