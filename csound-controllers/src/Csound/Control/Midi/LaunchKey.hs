module Csound.Control.Midi.LaunchKey(
    LkChn(..)
  , knob
  , knobs
  , idKnobs
  , knob'
  , knobs'
  , idKnobs'
  , tapBtn
  , tapBtns
  , tapBtnRow
  , tapBtnRowSig
  , toggleBtn
  , toggleBtns
) where

import Control.Monad
import Csound.Base hiding (knob, knob')

-- | Midi channel
newtype LkChn = LkChn Int

instance Default LkChn where
  def = LkChn 9

-- | Knob get unipolar value of the LK8 knob
--
-- > knob knobId initVal
--
-- knobId - [1, 16]
knob' :: LkChn -> Int -> D -> SE Sig
knob' (LkChn chn) n initValue = umidiCtrl (int chn) (getKnobId n) initValue

knobs' :: LkChn -> [D] -> SE [Sig]
knobs' chn = idKnobs' chn [1..]

idKnobs' :: LkChn -> [Int] -> [D] -> SE [Sig]
idKnobs' chn ids initVals = mapM (uncurry $ knob' chn) $ zip ids initVals

knob :: Int -> D -> SE Sig
knob = knob' def

knobs :: [D] -> SE [Sig]
knobs = knobs' def

idKnobs :: [Int] -> [D] -> SE [Sig]
idKnobs = idKnobs' def

getKnobId :: Int -> D
getKnobId n
  | 1 <= n && n <= 8  = int $ 20 + n
  | 9 <= n && n <= 16 = int $ 40 + (n - 8)
  | otherwise         = error "LK8 has only 16 knobs"

--------------------------

tapBtn :: LkChn -> Int -> SE Tick
tapBtn (LkChn n) idx = fmap (fmap $ const unit) $ midiKeyOn (Chn n) (getBtnNote idx)

tapBtns :: LkChn -> SE [Tick]
tapBtns chn = mapM (tapBtn chn) [1..8]

tapBtnRow :: LkChn -> SE (Evt D)
tapBtnRow chn = do
  evts <- tapBtns chn
  return $ mconcat $ zipWith (\n ev -> fmap (const n) ev) (fmap int [1..]) evts

tapBtnRowSig :: LkChn -> SE Sig
tapBtnRowSig = stepper 0 . fmap sig <=< tapBtnRow

getBtnNote :: Int -> D
getBtnNote n
  | 1 <= n && n <= 4 = int $ 8 + n
  | 5 <= n && n <= 8 = int $ 20 + n
  | otherwise        = error "LK8 has only 8 buttons"

toggleBtn :: LkChn -> Int -> SE (Evt D)
toggleBtn chn idx = fmap toTog $ tapBtn chn idx

toggleBtns :: LkChn -> SE [Evt D]
toggleBtns chn = mapM (toggleBtn chn) [1..8]


-- pressBtn :: Chn -> Int -> Evt Bool

-- pressBtnSig :: Chn -> Int -> SE Sig
-- toggleBtnSig :: Chn -> Int -> SE Sig