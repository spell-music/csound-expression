{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Csound.Exp.Numeric where

import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons 


instance Num E where    
    (+) = bi "+"
    (*) = bi "*"
    (-) = bi "-"    
    fromInteger = unSig . sig . int . fromInteger
    abs = opc1 "abs" idSignature
    signum = un

instance Num Sig where    
    (+) = bi "+"
    (*) = bi "*"
    (-) = bi "-"    
    fromInteger = sig . int . fromInteger
    abs = opc1 "abs" idSignature
    signum = un

instance Num Int' where
    (+) = bi "+"
    (*) = bi "*"
    (-) = bi "-"    
    fromInteger = int . fromInteger
    abs = opc1 "abs" idSignature
    signum = un

instance Num Double' where
    (+) = bi "+"
    (*) = bi "*"
    (-) = bi "-"    
    fromInteger = double . fromInteger
    abs = opc1 "abs" idSignature
    signum = un

instance Fractional Sig where
    (/) = bi "/"
    fromRational = sig . double . fromRational

instance Fractional Double' where
    (/) = bi "/"
    fromRational = double . fromRational


(!) :: Init a => Sig -> a -> Sig
(!) a b = wrap $ onExp phi $ unwrap a 
    where phi x = case x of
            Tfm t xs -> Tfm t (xs ++ [Fix $ unwrap b])
            x        -> x

