module Csound.Exp.Cons where

import Data.String
import Control.Applicative
import Data.Default
import qualified Data.Map as M
import Control.Monad.Trans.State

import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper


(!) :: Val a => Sig -> a -> Sig
(!) a b = wrap $ onExp phi $ unwrap a 
    where phi x = case x of
            Tfm t xs -> Tfm t (xs ++ [Fix $ unwrap b])
            x        -> x

------------------------------------------------
-- helper constructors

instance IsString String' where
    fromString = str

------------------------------------------------
-- constructor for simple arithmetic operators

bi :: (Val a1, Val a2, Val b) => Name -> a1 -> a2 -> b
bi name = opc2 name biSignature

biSignature :: Spec1
biSignature = [
    (Ar, [Ar, Ar]),
    (Kr, [Kr, Kr]),
    (Ir, [Ir, Ir])]

idSignature :: Spec1
idSignature = [
    (Ar, repeat Ar),
    (Kr, repeat Kr),
    (Ir, repeat Ir)]

----------------------------
-- spec tfms

tfms :: (Val a, Val b) => Info -> [a] -> b
tfms t as = tfm t $ map unwrap as

tfm0 :: (Val a) => Info -> a
tfm0 t = tfm t []

tfm1 :: (Val a, Val b) => Info -> a -> b
tfm1 t a = tfm t [unwrap a]

tfm2 :: (Val a1, Val a2, Val b) => Info -> a1 -> a2 -> b
tfm2 t a1 a2 = tfm t [unwrap a1, unwrap a2]

tfm3 :: (Val a1, Val a2, Val a3, Val b) => Info -> a1 -> a2 -> a3 -> b
tfm3 t a1 a2 a3 = tfm t [unwrap a1, unwrap a2, unwrap a3]

tfm4 :: (Val a1, Val a2, Val a3, Val a4, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> b
tfm4 t a1 a2 a3 a4 = tfm t [unwrap a1, unwrap a2, unwrap a3, unwrap a4]

tfm5 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> b
tfm5 t a1 a2 a3 a4 a5 = tfm t [unwrap a1, unwrap a2, unwrap a3, unwrap a4, unwrap a5]

tfm6 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
tfm6 t a1 a2 a3 a4 a5 a6 = tfm t [unwrap a1, unwrap a2, unwrap a3, unwrap a4, unwrap a5, unwrap a6]

tfm7 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
tfm7 t a1 a2 a3 a4 a5 a6 a7 = tfm t [unwrap a1, unwrap a2, unwrap a3, unwrap a4, unwrap a5, unwrap a6, unwrap a7]

-------------------------------
-- single out

type Spec1 = [(Rate, [Rate])]

spec1 :: Spec1 -> Signature
spec1 = SingleRate . M.fromList

opcs :: (Val a, Val b) => Name -> Spec1 -> [a] -> b
opcs name signature = tfms (pref name $ spec1 signature)

opc0 :: (Val a) => Name -> Spec1 -> a
opc0 name signature = tfm0 (pref name $ spec1 signature)

opc1 :: (Val a, Val b) => Name -> Spec1 -> a -> b
opc1 name signature = tfm1 (pref name $ spec1 signature)

opc2 :: (Val a1, Val a2, Val b) => Name -> Spec1 -> a1 -> a2 -> b
opc2 name signature = tfm2 (pref name $ spec1 signature)

opc3 :: (Val a1, Val a2, Val a3, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> b
opc3 name signature = tfm3 (pref name $ spec1 signature)

opc4 :: (Val a1, Val a2, Val a3, Val a4, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> b
opc4 name signature = tfm4 (pref name $ spec1 signature)

opc5 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> b
opc5 name signature = tfm5 (pref name $ spec1 signature)

opc6 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
opc6 name signature = tfm6 (pref name $ spec1 signature)

opc7 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
opc7 name signature = tfm7 (pref name $ spec1 signature)

-------------------------------
-- multiple outs

type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate 

mo :: (MultiOut a) => E -> a
mo = multiOuts

mopcs :: (Val a, MultiOut b) => Name -> Specs -> [a] -> b
mopcs name signature as = mo $ tfms (pref name $ specs signature) as

mopc0 :: (MultiOut a) => Name -> Specs -> a
mopc0 name signature = mo $ tfm0 (pref name $ specs signature)

mopc1 :: (Val a, MultiOut b) => Name -> Specs -> a -> b
mopc1 name signature a1 = mo $ tfm1 (pref name $ specs signature) a1

mopc2 :: (Val a1, Val a2, MultiOut b) => Name -> Specs -> a1 -> a2 -> b
mopc2 name signature a1 a2 = mo $ tfm2 (pref name $ specs signature) a1 a2

mopc3 :: (Val a1, Val a2, Val a3, MultiOut b) => Name -> Specs -> a1 -> a2 -> a3 -> b
mopc3 name signature a1 a2 a3 = mo $ tfm3 (pref name $ specs signature) a1 a2 a3

mopc4 :: (Val a1, Val a2, Val a3, Val a4, MultiOut b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> b
mopc4 name signature a1 a2 a3 a4 = mo $ tfm4 (pref name $ specs signature) a1 a2 a3 a4

mopc5 :: (Val a1, Val a2, Val a3, Val a4, Val a5, MultiOut b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> b
mopc5 name signature a1 a2 a3 a4 a5 = mo $ tfm5 (pref name $ specs signature) a1 a2 a3 a4 a5

mopc6 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, MultiOut b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
mopc6 name signature a1 a2 a3 a4 a5 a6 = mo $ tfm6 (pref name $ specs signature) a1 a2 a3 a4 a5 a6

mopc7 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, MultiOut b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
mopc7 name signature a1 a2 a3 a4 a5 a6 a7 = mo $ tfm7 (pref name $ specs signature) a1 a2 a3 a4 a5 a6 a7




