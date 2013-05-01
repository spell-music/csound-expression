{-# Language 
        TypeFamilies,
        FlexibleContexts #-}
module Csound.Exp.Tuple(
    CsdTuple(..), Out(..), multiOuts,

) where

import Control.Applicative(liftA2)
import Control.Monad(join)

import Csound.Exp
import Csound.Exp.Wrapper(Val(..), Sig, D, Str, Spec, SE, onExp, toExp, 
    withRate, getRates, isMultiOutSignature)

-- | Describes tuples of Csound values. It's used for functions that can return 
-- several results (such as 'soundin' or 'diskin2'). Tuples can be nested. 
class CsdTuple a where
    fromCsdTuple :: a -> [E]
    toCsdTuple :: [E] -> a
    arityCsdTuple :: a -> Int

-- | Output of the instrument.
class CsdTuple (NoSE a) => Out a where
    type NoSE a :: *
    toOut :: a -> SE [Sig]
    fromOut :: [Sig] -> a

-- CsdTuple instances

instance CsdTuple Sig where
    fromCsdTuple = return . toE
    toCsdTuple = fromE . head
    arityCsdTuple = const 1

instance CsdTuple D where
    fromCsdTuple = return . toE
    toCsdTuple = fromE . head
    arityCsdTuple = const 1

instance CsdTuple Tab where
    fromCsdTuple = return . toE
    toCsdTuple = fromE . head
    arityCsdTuple = const 1

instance CsdTuple Str where
    fromCsdTuple = return . toE
    toCsdTuple = fromE . head
    arityCsdTuple = const 1

instance CsdTuple Spec where
    fromCsdTuple = return . toE
    toCsdTuple = fromE . head
    arityCsdTuple = const 1

instance (CsdTuple a, CsdTuple b) => CsdTuple (a, b) where
    fromCsdTuple (a, b) = fromCsdTuple a ++ fromCsdTuple b
    arityCsdTuple x = let (a, b) = proxy x in arityCsdTuple a + arityCsdTuple b
        where proxy :: (a, b) -> (a, b)
              proxy = const (undefined, undefined)  
    toCsdTuple xs = (a, b)
        where a = toCsdTuple $ take (arityCsdTuple a) xs
              xsb = drop (arityCsdTuple a) xs  
              b = toCsdTuple (take (arityCsdTuple b) xsb)

instance (CsdTuple a, CsdTuple b, CsdTuple c) => CsdTuple (a, b, c) where
    fromCsdTuple (a, b, c) = fromCsdTuple (a, (b, c))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c) -> (a, (b, c))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c)) -> (a, b, c)) . toCsdTuple 

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d) => CsdTuple (a, b, c, d) where
    fromCsdTuple (a, b, c, d) = fromCsdTuple (a, (b, c, d))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d) -> (a, (b, c, d))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d)) -> (a, b, c, d)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e) => CsdTuple (a, b, c, d, e) where
    fromCsdTuple (a, b, c, d, e) = fromCsdTuple (a, (b, c, d, e))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e) -> (a, (b, c, d, e))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e)) -> (a, b, c, d, e)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f) => CsdTuple (a, b, c, d, e, f) where
    fromCsdTuple (a, b, c, d, e, f) = fromCsdTuple (a, (b, c, d, e, f))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e, f)) -> (a, b, c, d, e, f)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g) => CsdTuple (a, b, c, d, e, f, g) where
    fromCsdTuple (a, b, c, d, e, f, g) = fromCsdTuple (a, (b, c, d, e, f, g))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e, f, g)) -> (a, b, c, d, e, f, g)) . toCsdTuple

instance (CsdTuple a, CsdTuple b, CsdTuple c, CsdTuple d, CsdTuple e, CsdTuple f, CsdTuple g, CsdTuple h) => CsdTuple (a, b, c, d, e, f, g, h) where
    fromCsdTuple (a, b, c, d, e, f, g, h) = fromCsdTuple (a, (b, c, d, e, f, g, h))
    arityCsdTuple = arityCsdTuple . proxy
        where proxy :: (a, b, c, d, e, f, g, h) -> (a, (b, c, d, e, f, g, h))
              proxy = const undefined  
    toCsdTuple = (\(a, (b, c, d, e, f, g, h)) -> (a, b, c, d, e, f, g, h)) . toCsdTuple

------------------------------------------------
-- multiple outs

multiOuts :: CsdTuple a => E -> a
multiOuts exp = res
    where res = toCsdTuple $ multiOutsSection (arityCsdTuple res) exp

multiOutsSection :: Int -> E -> [E]
multiOutsSection n e = zipWith (\n r -> select n r e') [0 ..] rates
    where rates = take n $ getRates $ toExp e          
          e' = onExp (setMultiRate rates) e
          
          setMultiRate rates (Tfm info xs) = Tfm (info{ infoSignature = MultiRate rates ins }) xs 
              where MultiRate _ ins = infoSignature info
            
          select n r e = withRate r $ Select r n (PrimOr $ Right e)

------------------------------------------------
-- instrument outs

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

