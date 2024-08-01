{- | Tools to build Fm synthesis graphs

Example

> f a = fmOut1 $ do
>  x1 <- fmOsc 1
>  x2 <- fmOsc 2
>  x1 `fmod` [(a, x2)]
>  return x1
-}
module Csound.Air.Fm (
  -- * Fm graph
  Fm,
  FmNode,
  fmOsc',
  fmOsc,
  fmSig,
  fmod,
  fmOut,
  fmOut1,
  fmOut2,

  -- * Simplified Fm graph
  FmSpec (..),
  FmGraph (..),
  fmRun,

  -- ** Specific graphs

  -- | Algorithms for DX7 fm synth
  dx_1,
  dx_2,
  dx_3,
  dx_4 {-,  dx_5,  dx_6,  dx_7,  dx_8,
       dx_9,  dx_10, dx_11, dx_12, dx_13, dx_14, dx_15, dx_16,
       dx_17, dx_18, dx_19, dx_20, dx_21, dx_22, dx_23, dx_24,
       dx_25, dx_26, dx_27, dx_28, dx_29, dx_30, dx_31, dx_32 -},
) where

import Data.IntMap qualified as IM

import Control.Monad
import Control.Monad.Trans.State.Strict

import Csound.Air.Wave
import Csound.Typed

-- Fm graph rendering

type Fm a = State St a

newtype FmNode = FmNode Int

type FmIdx = (Int, Sig)

data Fmod = Fmod (Sig -> SE Sig) Sig [FmIdx] | Fsig Sig

data St = St
  { st'newIdx :: Int
  , st'units :: [Fmod]
  , st'links :: IM.IntMap [FmIdx]
  }

defSt :: St
defSt =
  St
    { st'newIdx = 0
    , st'units = []
    , st'links = IM.empty
    }

renderGraph :: [Fmod] -> [FmIdx] -> Sig -> SE [Sig]
renderGraph units outs cps = do
  refs <- initUnits (length units)
  mapM_ (loopUnit refs) (zip [0 ..] units)
  mapM (renderIdx refs) outs
  where
    initUnits n = mapM (const $ newRef (0 :: Sig)) [1 .. n]

    loopUnit refs (n, x) =
      writeRef (refs !! n) =<< case x of
        Fsig asig -> return asig
        Fmod wave modFreq subs -> do
          s <- fmap sum $ mapM (renderModIdx refs) subs
          wave (cps * modFreq + s)
      where

    renderIdx :: [Ref Sig] -> (Int, Sig) -> SE Sig
    renderIdx refs (idx, amp) = mul amp $ readRef (refs !! idx)

    renderModIdx :: [Ref Sig] -> (Int, Sig) -> SE Sig
    renderModIdx refs (idx, amp) = mul (amp * modFreq) $ readRef (refs !! idx)
      where
        modFreq = case (units !! idx) of
          Fmod _ m _ -> m * cps
          _ -> 1

mkGraph :: St -> [Fmod]
mkGraph s = zipWith extractMod (reverse $ st'units s) [0 ..]
  where
    extractMod x n = case x of
      Fmod alg w _ -> Fmod alg w (maybe [] id $ IM.lookup n (st'links s))
      _ -> x

toFmIdx :: (Sig, FmNode) -> FmIdx
toFmIdx (amp, FmNode n) = (n, amp)

---------------------------------------------------------
-- constructors

{- | Creates fm node with generic wave.

> fmOsc' wave modFreq
-}
fmOsc' :: (Sig -> SE Sig) -> Sig -> Fm FmNode
fmOsc' wave idx = newFmod (Fmod wave idx [])

{- | Creates fm node with sine wave.

> fmOsc modFreq
-}
fmOsc :: Sig -> Fm FmNode
fmOsc = fmOsc' rndOsc

-- | Creates fm node with signal generator (it's independent from the main frequency).
fmSig :: Sig -> Fm FmNode
fmSig a = newFmod (Fsig a)

newFmod :: Fmod -> Fm FmNode
newFmod a = state $ \s ->
  let
    n = st'newIdx s
    s1 = s{st'newIdx = n + 1, st'units = a : st'units s}
   in
    (FmNode n, s1)

-- modulator

fmod :: FmNode -> [(Sig, FmNode)] -> Fm ()
fmod (FmNode idx) mods = state $ \s ->
  ((), s{st'links = IM.insertWithKey (\_ a b -> a ++ b) idx (fmap toFmIdx mods) (st'links s)})

-- outputs

-- | Renders Fm synth to function.
fmOut :: Fm [(Sig, FmNode)] -> Sig -> SE [Sig]
fmOut fm = renderGraph (mkGraph s) (fmap toFmIdx outs)
  where
    (outs, s) = runState fm defSt

-- | Renders mono output.
fmOut1 :: Fm FmNode -> Sig -> SE Sig
fmOut1 fm cps = fmap head $ fmOut (fmap (\x -> [(1, x)]) fm) cps

-- | Renders stereo output.
fmOut2 :: Fm (FmNode, FmNode) -> Sig -> SE Sig2
fmOut2 fm cps = fmap (\[a, b] -> (a, b)) $ fmOut (fmap (\(a, b) -> [(1, a), (1, b)]) fm) cps

-----------------------------------------------------------------------

data FmSpec = FmSpec
  { fmWave :: [Sig -> SE Sig]
  , fmCps :: [Sig]
  , fmInd :: [Sig]
  , fmOuts :: [Sig]
  }

data FmGraph = FmGraph
  { fmGraph :: [(Int, [Int])]
  , fmGraphOuts :: [Int]
  }

fmRun :: FmGraph -> FmSpec -> Sig -> SE Sig
fmRun graph spec' cps = fmap sum $ ($ cps) $ fmOut $ do
  ops <- zipWithM fmOsc' (fmWave spec) (fmCps spec)
  mapM_ (mkMod ops (fmInd spec)) (fmGraph graph)
  return $ zipWith (toOut ops) (fmOuts spec) (fmGraphOuts graph)
  where
    spec = addDefaults spec'
    toOut xs amp n = (amp, xs !! n)
    mkMod ops ixs (n, ms) = (ops !! n) `fmod` (fmap (\m -> (ixs !! m, ops !! m)) ms)

addDefaults :: FmSpec -> FmSpec
addDefaults spec =
  spec
    { fmWave = fmWave spec ++ repeat rndOsc
    , fmCps = fmCps spec ++ repeat 1
    , fmInd = fmInd spec ++ repeat 1
    , fmOuts = fmOuts spec ++ repeat 1
    }

{- |
>   +--+
>   6  |
>   +--+
>   5
>   |
> 2 4
> | |
> 1 3
> +---+
-}
dx_1 :: FmGraph
dx_1 =
  FmGraph
    { fmGraphOuts = [1, 3]
    , fmGraph =
        [ (1, [2])
        , (3, [4])
        , (4, [5])
        , (5, [6])
        , (6, [6])
        ]
    }

{- |
>         6
>         |
>         5
>   +--+  |
> 2  |  4
> +--+  |
> 1     3
>   +-----+
-}
dx_2 :: FmGraph
dx_2 =
  FmGraph
    { fmGraphOuts = [1, 3]
    , fmGraph =
        [ (1, [2])
        , (2, [2])
        , (3, [4])
        , (5, [6])
        ]
    }

{- |
>     +--+
> 3   6  |
> |   +--+
> 2   5
> | |
> 1   4
> +---+
-}
dx_3 :: FmGraph
dx_3 =
  FmGraph
    { fmGraphOuts = [1, 4]
    , fmGraph =
        [ (1, [2])
        , (2, [3])
        , (4, [5])
        , (5, [6])
        , (6, [6])
        ]
    }

{- |
>     +--+
>   3 6  |
>   | |  |
>   2 5  |
>   | |  |
>   1 4  |
>   | +--+
>       +---+
-}
dx_4 :: FmGraph
dx_4 =
  FmGraph
    { fmGraphOuts = [1, 4]
    , fmGraph =
        [ (1, [2])
        , (2, [3])
        , (4, [5])
        , (5, [6])
        , (6, [4])
        ]
    }

{-
dx12 = DxGraph
  { dxGraphOuts = [3, 1]
  , dxGraph =
    [ (3, [4, 5, 6])
    , (1, [2])
    , (2, [2]) ]}

-}
