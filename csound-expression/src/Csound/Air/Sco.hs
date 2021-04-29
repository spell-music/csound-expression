-- | Utils for Scores
module Csound.Air.Sco(
    euc, dot, ddot, scoBpm, trn,
    -- * Shortcuts
    -- | Naming conventions :
    --
    -- First part @x@ can be [b | w | h | q | e | s | t | d[x] ]
    --
    -- @b@ means brewis @(str 2)@
    --
    -- @w@ means whole @(str 1)@
    --
    -- @h@ means half @(str $ 1/2)@
    --
    -- @q@ means quater @(str $ 1/4)@
    --
    -- @e@ means eighth @(str $ 1/8)@
    --
    -- @s@ means sixteenth @(str $ 1/16)@
    --
    -- @t@ means thirty second @(str $ 1/32)@
    --
    -- @d[x]@ means dotted [x] @(str 1.5 $ x)@
    bn, wn, hn, qn, en, sn, tn,

    -- ** Pauses
    -- | Naming conventions are the same as for 'time string'.
    bnr, wnr, hnr, qnr, enr, snr, tnr
) where

import Csound.Typed.Types
import Csound.Typed.Control
import Temporal.Media

-- | Euclidean beats.
--
-- Scales series of scores by apllying series of stretching transformations.
--
-- > euc totalLength initDelay durations scores
euc :: Double -> Double -> [Double] -> [Sco a] -> Sco a
euc len delTime durs scos =
  go (delTime, rest (sig $ double delTime)) $ zip (cycle durs) (cycle scos)
  where
    go :: (Double, Sco a) -> [(Double, Sco  a)] -> Sco a
    go (time, res) xs = case xs of
      []             -> res
      (dt, a) : tl ->
        let nextTime = time + dt
        in  if nextTime < len
              then go (nextTime, mel [res, str (sig $ double dt) a]) tl
              else let dtReduced = len - time
                   in  mel [res, str (sig $ double dtReduced) a]

-- | Sets tempo in beats per minute,
-- if 1 "Dur" is equal to 1 second before transformation.
scoBpm :: Sig -> (Sco a -> Sco a)
scoBpm beat = str (x1/x0)
    where x0 = 0.25
          x1 = 60/beat

-- | Means 'three notes'. Plays three notes as fast as two.
trn :: Sco a -> Sco a
trn = str (2/3)

bn, wn, hn, qn, en, sn, tn  :: Sco a -> Sco a

bn = str 2
wn = id
hn = str $ 1/2
qn = str $ 1/4
en = str $ 1/8
sn = str $ 1/16
tn = str $ 1/32

-- | Synonym to @'str' (3/2)@
dot :: Sco a -> Sco a
dot = str $ 3/2

-- | double 'dot', str with 1.75
ddot :: Sco a -> Sco a
ddot = str 1.75

bnr, wnr, hnr, qnr, enr, snr, tnr :: Sco a

wnr = rest 1

bnr = bn wnr
hnr = hn wnr
qnr = qn wnr
enr = en wnr
snr = sn wnr
tnr = tn wnr


