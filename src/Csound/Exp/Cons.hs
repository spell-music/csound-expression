-- |  Constructors
module Csound.Exp.Cons (
    Spec1, Specs,
    withInits,
    bi,
    spec1, specs,
    opcs, opc0, opc1, opc2, opc3, opc4, opc5, opc6, opc7, opc8, opc9, opc10, opc11, opc12,
    mopcs, mopc0, mopc1, mopc2, mopc3, mopc4, mopc5, mopc6, mopc7
) where

import qualified Data.Map as M(fromList)

import Csound.Exp
import Csound.Exp.Wrapper(Sig, Val(..), tfm, pref, onExp)
import Csound.Exp.Tuple(CsdTuple, fromCsdTuple, multiOuts)

-- | Appends initialisation arguments. It's up to you to supply arguments with the right types. For example:
--
-- > oscil 0.5 440 sinWave `withInits` (0.5 :: D)
withInits :: (Val a, CsdTuple inits) => a -> inits -> Sig
withInits a b = fromE $ onExp phi $ toE a
    where phi x = case x of
            Tfm t xs -> Tfm t (xs ++ (fmap toPrimOr $ fromCsdTuple b))
            _        -> x

------------------------------------------------
-- constructor for simple arithmetic operators

bi :: (Val a1, Val a2, Val b) => Name -> a1 -> a2 -> b
bi name = opc2 name biSignature

biSignature :: Spec1
biSignature = [
    (Ar, [Ar, Ar]),
    (Kr, [Kr, Kr]),
    (Ir, [Ir, Ir])]

----------------------------
-- constructors for opcodes (single or multiple rates)

tfms :: (Val a, Val b) => Info -> [a] -> b
tfms t as = tfm t $ map toE as

tfm0 :: (Val a) => Info -> a
tfm0 t = tfm t []

tfm1 :: (Val a, Val b) => Info -> a -> b
tfm1 t a = tfm t [toE a]

tfm2 :: (Val a1, Val a2, Val b) => Info -> a1 -> a2 -> b
tfm2 t a1 a2 = tfm t [toE a1, toE a2]

tfm3 :: (Val a1, Val a2, Val a3, Val b) => Info -> a1 -> a2 -> a3 -> b
tfm3 t a1 a2 a3 = tfm t [toE a1, toE a2, toE a3]

tfm4 :: (Val a1, Val a2, Val a3, Val a4, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> b
tfm4 t a1 a2 a3 a4 = tfm t [toE a1, toE a2, toE a3, toE a4]

tfm5 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> b
tfm5 t a1 a2 a3 a4 a5 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5]

tfm6 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
tfm6 t a1 a2 a3 a4 a5 a6 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6]

tfm7 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
tfm7 t a1 a2 a3 a4 a5 a6 a7 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7]

tfm8 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b
tfm8 t a1 a2 a3 a4 a5 a6 a7 a8 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7, toE a8]

tfm9 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b
tfm9 t a1 a2 a3 a4 a5 a6 a7 a8 a9 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7, toE a8, toE a9]

tfm10 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> b
tfm10 t a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7, toE a8, toE a9, toE a10]

tfm11 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val a11, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> b
tfm11 t a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7, toE a8, toE a9, toE a10, toE a11]

tfm12 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val a11, Val a12, Val b) => Info -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> b
tfm12 t a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = tfm t [toE a1, toE a2, toE a3, toE a4, toE a5, toE a6, toE a7, toE a8, toE a9, toE a10, toE a11, toE a12]

-------------------------------
-- single output

-- User friendly type for single output type signatures
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

opc8 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b
opc8 name signature = tfm8 (pref name $ spec1 signature)

opc9 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b
opc9 name signature = tfm9 (pref name $ spec1 signature)

opc10 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> b
opc10 name signature = tfm10 (pref name $ spec1 signature)

opc11 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val a11, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> b
opc11 name signature = tfm11 (pref name $ spec1 signature)

opc12 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, Val a8, Val a9, Val a10, Val a11, Val a12, Val b) => Name -> Spec1 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> b
opc12 name signature = tfm12 (pref name $ spec1 signature)


-------------------------------
-- multiple outputs

-- User friendly type for multiple outputs type signatures
type Specs = ([Rate], [Rate])

specs :: Specs -> Signature
specs = uncurry MultiRate 

mo :: (CsdTuple a) => E -> a
mo = multiOuts

mopcs :: (Val a, CsdTuple b) => Name -> Specs -> [a] -> b
mopcs name signature as = mo $ tfms (pref name $ specs signature) as

mopc0 :: (CsdTuple a) => Name -> Specs -> a
mopc0 name signature = mo $ tfm0 (pref name $ specs signature)

mopc1 :: (Val a, CsdTuple b) => Name -> Specs -> a -> b
mopc1 name signature a1 = mo $ tfm1 (pref name $ specs signature) a1

mopc2 :: (Val a1, Val a2, CsdTuple b) => Name -> Specs -> a1 -> a2 -> b
mopc2 name signature a1 a2 = mo $ tfm2 (pref name $ specs signature) a1 a2

mopc3 :: (Val a1, Val a2, Val a3, CsdTuple b) => Name -> Specs -> a1 -> a2 -> a3 -> b
mopc3 name signature a1 a2 a3 = mo $ tfm3 (pref name $ specs signature) a1 a2 a3

mopc4 :: (Val a1, Val a2, Val a3, Val a4, CsdTuple b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> b
mopc4 name signature a1 a2 a3 a4 = mo $ tfm4 (pref name $ specs signature) a1 a2 a3 a4

mopc5 :: (Val a1, Val a2, Val a3, Val a4, Val a5, CsdTuple b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> b
mopc5 name signature a1 a2 a3 a4 a5 = mo $ tfm5 (pref name $ specs signature) a1 a2 a3 a4 a5

mopc6 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, CsdTuple b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
mopc6 name signature a1 a2 a3 a4 a5 a6 = mo $ tfm6 (pref name $ specs signature) a1 a2 a3 a4 a5 a6

mopc7 :: (Val a1, Val a2, Val a3, Val a4, Val a5, Val a6, Val a7, CsdTuple b) => Name -> Specs -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
mopc7 name signature a1 a2 a3 a4 a5 a6 a7 = mo $ tfm7 (pref name $ specs signature) a1 a2 a3 a4 a5 a6 a7

