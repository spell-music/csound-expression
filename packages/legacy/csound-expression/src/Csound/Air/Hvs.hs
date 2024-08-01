-- | Hyper vectorial synthesis
module Csound.Air.Hvs (
  HvsSnapshot,
  HvsMatrix1,
  HvsMatrix2,
  HvsMatrix3,
  hvs1,
  hvs2,
  hvs3,
  -- | Csound functions
  csdHvs1,
  csdHvs2,
  csdHvs3,
) where

import Control.Monad.Trans.Class
import Csound.Dynamic hiding (int)
import Csound.Typed

import Csound.Typed.Opcode hiding (hvs1, hvs2, hvs3)

import Csound.Tab

-- | Hvs vector
type HvsSnapshot = [Double]

-- | 1D matrix
type HvsMatrix1 = [HvsSnapshot]

-- | 2D matrix (grid of vecotrs)
type HvsMatrix2 = [HvsMatrix1]

-- | 3D matrix (cube of vectors)
type HvsMatrix3 = [HvsMatrix2]

-- Hyper Vectorial Synthesis.

{- |
Allows one-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs1 allows one-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs1  kx, inumParms, inumPointsX, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <http://www.csounds.com/manual/html/hvs1.html>
-}
csdHvs1 :: Sig -> D -> D -> Tab -> Tab -> Tab -> SE ()
csdHvs1 b1 b2 b3 b4 b5 b6 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unD b2 <*> unD b3 <*> unTab b4 <*> unTab b5 <*> unTab b6
  where
    f a1 a2 a3 a4 a5 a6 = opcs "hvs1" [(Xr, [Kr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Allows two-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs2 allows two-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs2  kx, ky, inumParms, inumPointsX, inumPointsY, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <http://www.csounds.com/manual/html/hvs2.html>
-}
csdHvs2 :: Sig -> Sig -> D -> D -> D -> Tab -> Tab -> Tab -> SE ()
csdHvs2 b1 b2 b3 b4 b5 b6 b7 b8 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> unTab b6 <*> unTab b7 <*> unTab b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcs
        "hvs2"
        [(Xr, [Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])]
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        ]

{- |
Allows three-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

hvs3 allows three-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.

>  hvs3  kx, ky, kz, inumParms, inumPointsX, inumPointsY, inumPointsZ, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]

csound doc: <http://www.csounds.com/manual/html/hvs3.html>
-}
csdHvs3 :: Sig -> Sig -> Sig -> D -> D -> D -> D -> Tab -> Tab -> Tab -> SE ()
csdHvs3 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unD b4 <*> unD b5 <*> unD b6 <*> unD b7 <*> unTab b8 <*> unTab b9 <*> unTab b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      opcs
        "hvs3"
        [
          ( Xr
          , [Kr, Kr, Kr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
          )
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

{- | One dimensional Hyper vectorial synthesis.
We can provide a list of vectors (of lists but the same length for all items is assumed)
and a signal that ranges from 0 to 1. It interpolates between vectors in the list.
As a result we get a n interpolated vector. It's a list but the actual length
equals to the length of input vectors.

An example. We can set the center frequency and resonance of the filter with the single parameter:

> let f = hvs1 [[100, 0.1], [300, 0.1], [600, 0.5], [800, 0.9]]
>  dac $ lift1 (\x -> fmap (\[cps, q] -> mlp cps q (saw 110)) $ f x) (uknob 0.5)

Notice the exact pattern match with the list in the argument of the lambda function:

> \[cps, q] -> mlp cps q (saw 110)) $ f x

It's determined by the length of the items in the input list.
-}
hvs1 :: HvsMatrix1 -> Sig -> SE [Sig]
hvs1 as x = do
  outTab <- newTab (int numParams)
  csdHvs1 x (int numParams) (int numPointsX) outTab positionsTab snapTab
  return $ fmap (kr . flip tab outTab . sig . int) [0 .. numParams - 1]
  where
    numParams = length $ head as
    numPointsX = length as

    positionsTab = doubles $ fmap fromIntegral [0 .. numPointsX - 1]
    snapTab = doubles $ concat as

{- | Two dimensional Hyper vectorial synthesis.
Now we provide a list of lists of vectors. The length of all vectors should be the same
but there is no limit for the number! So that's how we can control a lot of parameters
with pair of signals. The input 2D atrix is the grid of samples.
It finds the closest four points in the grid and interpolates between them (it's a weighted sum).

> hvs2 matrix (x, y)

The usage is the same as in the case of @hvs1@. An example:

> g = hvs2 [[[100, 0.1, 0.3], [800, 0.1, 0.5], [1400, 0.1, 0.8]],
>      [[100, 0.5, 0.3], [800, 0.5, 0.5], [1400, 0.5, 0.8]],
>      [[100, 0.8, 0.3], [800, 0.8, 0.5], [1400, 0.8, 0.8]]]
>
> main = dac $ do
>  (g1, kx) <- uknob 0.5
>  (g2, ky) <- uknob 0.5
>  [cfq, q, w] <- g (kx, ky)
>  panel $ hor [g1, g2]
>  at (mlp cfq q) $ fmap (cfd w (saw 110)) (white)
-}
hvs2 :: HvsMatrix2 -> Sig2 -> SE [Sig]
hvs2 as (x, y) = do
  outTab <- newTab (int numParams)
  csdHvs2 x y (int numParams) (int numPointsX) (int numPointsY) outTab positionsTab snapTab
  return $ fmap (kr . flip tab outTab . sig . int) [0 .. numParams - 1]
  where
    numParams = length $ head $ head as
    numPointsX = length $ head as
    numPointsY = length as

    positionsTab = doubles $ fmap fromIntegral [0 .. (numPointsX * numPointsY - 1)]
    snapTab = doubles $ concat $ concat as

-- | The three dimensional
hvs3 :: HvsMatrix3 -> Sig3 -> SE [Sig]
hvs3 as (x, y, z) = do
  outTab <- newTab (int numParams)
  csdHvs3 x y z (int numParams) (int numPointsX) (int numPointsY) (int numPointsZ) outTab positionsTab snapTab
  return $ fmap (kr . flip tab outTab . sig . int) [0 .. numParams - 1]
  where
    numParams = length $ head $ head $ head as
    numPointsX = length $ head $ head as
    numPointsY = length $ head as
    numPointsZ = length as

    positionsTab = doubles $ fmap fromIntegral [0 .. (numPointsX * numPointsY * numPointsZ) - 1]
    snapTab = doubles $ concat $ concat $ concat as
