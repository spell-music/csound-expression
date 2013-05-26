{-# Language 
        TypeFamilies,
        FlexibleContexts #-}
module Csound.Exp.Tuple(
    CsdTuple(..), 
    fromCsdTuple, toCsdTuple, arityCsdTuple, ratesCsdTuple, defCsdTuple,
    Out(..), multiOuts, outArity
) where

import Data.Default

import Control.Applicative(liftA2)
import Control.Monad(join)

import Csound.Exp
import Csound.Exp.Wrapper(Val(..), Sig, D, Str, Spec, onExp, toExp, 
    withRate, getRates)
import Csound.Exp.SE(SE)

-- | Describes tuples of Csound values. It's used for functions that can return 
-- several results (such as 'soundin' or 'diskin2'). Tuples can be nested. 
class CsdTuple a where
    csdTupleMethods :: CsdTupleMethods a

data CsdTupleMethods a = CsdTupleMethods
    { fromCsdTuple_  :: a -> [E]
    , toCsdTuple_    :: [E] -> a
    , arityCsdTuple_ :: a -> Int
    , ratesCsdTuple_ :: a -> [Rate]
    , defCsdTuple_   :: a }

fromCsdTuple :: CsdTuple a => a -> [E] 
fromCsdTuple = fromCsdTuple_ csdTupleMethods

toCsdTuple :: CsdTuple a => [E] -> a
toCsdTuple = toCsdTuple_ csdTupleMethods

arityCsdTuple :: CsdTuple a => a -> Int
arityCsdTuple = arityCsdTuple_ csdTupleMethods

ratesCsdTuple :: CsdTuple a => a -> [Rate]
ratesCsdTuple = ratesCsdTuple_ csdTupleMethods

defCsdTuple :: CsdTuple a => a
defCsdTuple = defCsdTuple_ csdTupleMethods

-- | Defines instance of type class 'Arg' for a new type in terms of an already defined one.
makeCsdTupleMethods :: (CsdTuple a) => (a -> b) -> (b -> a) -> CsdTupleMethods b
makeCsdTupleMethods to from = CsdTupleMethods 
    { fromCsdTuple_  = fromCsdTuple . from
    , toCsdTuple_    = to . toCsdTuple 
    , arityCsdTuple_ = const $ arityCsdTuple $ proxy to
    , ratesCsdTuple_ = ratesCsdTuple . from
    , defCsdTuple_   = to defCsdTuple }
    where proxy :: (a -> b) -> a
          proxy = undefined

-- | Output of the instrument.
class CsdTuple (NoSE a) => Out a where
    type NoSE a :: *
    toOut :: a -> SE [Sig]
    fromOut :: [Sig] -> a

outArity :: Out a => a -> Int
outArity a = arityCsdTuple (proxy a)
    where proxy :: Out a => a -> NoSE a
          proxy = undefined  

-- CsdTuple instances

instance CsdTuple () where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_  = return []
        , toCsdTuple_    = const ()
        , arityCsdTuple_ = const 0
        , ratesCsdTuple_ = const []
        , defCsdTuple_   = () }

instance CsdTuple Sig where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_ = return . toE
        , toCsdTuple_ = fromE . head
        , arityCsdTuple_ = const 1
        , ratesCsdTuple_ = const [Ar]
        , defCsdTuple_   = def }
        
instance CsdTuple D where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_ = return . toE
        , toCsdTuple_ = fromE . head
        , arityCsdTuple_ = const 1
        , ratesCsdTuple_ = const [Ir]
        , defCsdTuple_   = def }

instance CsdTuple Tab where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_ = return . toE
        , toCsdTuple_ = fromE . head
        , arityCsdTuple_ = const 1
        , ratesCsdTuple_ = const [Ir]
        , defCsdTuple_   = def }

instance CsdTuple Str where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_ = return . toE
        , toCsdTuple_ = fromE . head
        , arityCsdTuple_ = const 1
        , ratesCsdTuple_ = const [Sr]
        , defCsdTuple_   = def }

instance CsdTuple Spec where
    csdTupleMethods = CsdTupleMethods 
        { fromCsdTuple_ = return . toE
        , toCsdTuple_ = fromE . head
        , arityCsdTuple_ = const 1
        , ratesCsdTuple_ = const [Fr]
        , defCsdTuple_   = def }

instance (CsdTuple a, CsdTuple b) => CsdTuple (a, b) where    
    csdTupleMethods = CsdTupleMethods fromCsdTuple' toCsdTuple' arityCsdTuple' ratesCsdTuple' defCsdTuple'
        where 
            fromCsdTuple' (a, b) = fromCsdTuple a ++ fromCsdTuple b
            arityCsdTuple' x = let (a, b) = proxy x in arityCsdTuple a + arityCsdTuple b
                where proxy :: (a, b) -> (a, b)
                      proxy = const (undefined, undefined)  
            toCsdTuple' xs = (a, b)
                where a = toCsdTuple $ take (arityCsdTuple a) xs
                      xsb = drop (arityCsdTuple a) xs  
                      b = toCsdTuple (take (arityCsdTuple b) xsb)

            ratesCsdTuple' (a, b) = ratesCsdTuple a ++ ratesCsdTuple b
            defCsdTuple' = (defCsdTuple, defCsdTuple)

instance (CsdTuple a, CsdTuple b, CsdTuple c) => CsdTuple (a, b, c) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c)) = (a, b, c)
              from (a, b, c) = (a, (b, c))  

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d) => CsdTuple (a, b, c, d) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c, d)) = (a, b, c, d)
              from (a, b, c, d) = (a, (b, c, d))  

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e) => CsdTuple (a, b, c, d, e) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c, d, e)) = (a, b, c, d, e)
              from (a, b, c, d, e) = (a, (b, c, d, e))  

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f) => CsdTuple (a, b, c, d, e, f) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c, d, e, f)) = (a, b, c, d, e, f)
              from (a, b, c, d, e, f) = (a, (b, c, d, e, f))  

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g) => CsdTuple (a, b, c, d, e, f, g) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c, d, e, f, g)) = (a, b, c, d, e, f, g)
              from (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))  

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h) => CsdTuple (a, b, c, d, e, f, g, h) where
    csdTupleMethods = makeCsdTupleMethods to from
        where to (a, (b, c, d, e, f, g, h)) = (a, b, c, d, e, f, g, h)
              from (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))  

------------------------------------------------
-- multiple outs

multiOuts :: CsdTuple a => E -> a
multiOuts expr = res
    where res = toCsdTuple $ multiOutsSection (arityCsdTuple res) expr

multiOutsSection :: Int -> E -> [E]
multiOutsSection n e = zipWith (\cellId r -> select cellId r e') [0 ..] outRates
    where outRates = take n $ getRates $ toExp e          
          e' = onExp (setMultiRate outRates) e
          
          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs 
              where ins = case infoSignature info of
                        MultiRate _ a -> a
                        _ -> error "Tuple.hs: multiOutsSection -- should be multiOut expression" 
          setMultiRate _ _ = error "Tuple.hs: multiOutsSection -- argument should be Tfm-expression"  
            
          select cellId rate expr = withRate rate $ Select rate cellId (PrimOr $ Right expr)

------------------------------------------------
-- instrument outs

instance Out () where
    type NoSE () = ()
    toOut = const (return [])
    fromOut = const ()

instance Out Sig where
    type NoSE Sig = Sig
    toOut = return . return
    fromOut = head  

instance (Out a, CsdTuple a) => Out (SE a) where
    type NoSE (SE a) = a
    toOut = join . fmap toOut
    fromOut = return . fromOut


instance (CsdTuple a, CsdTuple b, Out a, Out b) => Out (a, b) where
    type NoSE (a, b) = (NoSE a, NoSE b)
    toOut (a, b) = liftA2 (++) (toOut a) (toOut b)
    fromOut = toCsdTuple . fmap toE

    
instance (CsdTuple a, CsdTuple b, CsdTuple c, Out a, Out b, Out c) => Out (a, b, c) where
    type NoSE (a, b, c) = (NoSE a, NoSE b, NoSE c)
    toOut (a, b, c) = toOut (a, (b, c))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, Out a, Out b, Out c, Out d) => Out (a, b, c, d) where
    type NoSE (a, b, c, d) = (NoSE a, NoSE b, NoSE c, NoSE d)
    toOut (a, b, c, d) = toOut (a, (b, c, d))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, Out a, Out b, Out c, Out d, Out e) => Out (a, b, c, d, e) where
    type NoSE (a, b, c, d, e) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e)
    toOut (a, b, c, d, e) = toOut (a, (b, c, d, e))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, Out a, Out b, Out c, Out d, Out e, Out f) => Out (a, b, c, d, e, f) where
    type NoSE (a, b, c, d, e, f) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f)
    toOut (a, b, c, d, e, f) = toOut (a, (b, c, d, e, f))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, Out a, Out b, Out c, Out d, Out e, Out f, Out g) => Out (a, b, c, d, e, f, g) where
    type NoSE (a, b, c, d, e, f, g) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g)
    toOut (a, b, c, d, e, f, g) = toOut (a, (b, c, d, e, f, g))
    fromOut = toCsdTuple . fmap toE
    
instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h, Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h) => Out (a, b, c, d, e, f, g, h) where
    type NoSE (a, b, c, d, e, f, g, h) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g, NoSE h)
    toOut (a, b, c, d, e, f, g, h) = toOut (a, (b, c, d, e, f, g, h))
    fromOut = toCsdTuple . fmap toE

