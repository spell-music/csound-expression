-- | Functions to play with Novation LaunchKey midi-controller
module Csound.Control.Midi.LaunchKey (
  LkChn (..),
  knob,
  knobs,
  idKnobs,
  knob',
  knobs',
  idKnobs',
  tapBtn,
  tapBtns,
  tapBtnRow,
  tapBtnRowSig,
  toggleBtn,
  toggleBtns,

  -- * arrow buttons

  -- | Note that we need to set +-raw_controller_mode=1
  -- to use these buttons in Csound options/flags.

  -- ** Signal producers (1 / 0)
  arrowUpSig,
  arrowDownSig,
  arrowLeftSig,
  arrowRightSig,

  -- ** Sense taps (only ON press)
  arrowUpTap,
  arrowDownTap,
  arrowLeftTap,
  arrowRightTap,

  -- ** Sense on/off taps (1 for ON and 0 for OFF)
  arrowUpToggle,
  arrowDownToggle,
  arrowLeftToggle,
  arrowRightToggle,
) where

import Control.Monad
import Csound.Base hiding (knob, knob')

-- | Midi channel
newtype LkChn = LkChn Int

instance Default LkChn where
  def = LkChn 9

{- | Knob get unipolar value of the LK8 knob

> knob knobId initVal

knobId - [1, 16]
-}
knob' :: LkChn -> Int -> D -> SE Sig
knob' (LkChn chn) n initValue = umidiCtrl (int chn) (getKnobId n) initValue

knobs' :: LkChn -> [D] -> SE [Sig]
knobs' chn = idKnobs' chn [1 ..]

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
  | 1 <= n && n <= 8 = int $ 20 + n
  | 9 <= n && n <= 16 = int $ 40 + (n - 8)
  | otherwise = error "LK8 has only 16 knobs"

--------------------------

tapBtn :: LkChn -> Int -> SE Tick
tapBtn (LkChn n) idx = fmap (fmap $ const unit) $ midiKeyOn (Chn n) (getBtnNote idx)

tapBtns :: LkChn -> SE [Tick]
tapBtns chn = mapM (tapBtn chn) [1 .. 8]

tapBtnRow :: LkChn -> SE (Evt D)
tapBtnRow chn = do
  evts <- tapBtns chn
  return $ mconcat $ zipWith (\n ev -> fmap (const n) ev) (fmap int [1 ..]) evts

tapBtnRowSig :: LkChn -> SE Sig
tapBtnRowSig = stepper 0 . fmap sig <=< tapBtnRow

getBtnNote :: Int -> D
getBtnNote n
  | 1 <= n && n <= 4 = int $ 8 + n
  | 5 <= n && n <= 8 = int $ 20 + n
  | otherwise = error "LK8 has only 8 buttons"

toggleBtn :: LkChn -> Int -> SE (Evt D)
toggleBtn chn idx = fmap toTog $ tapBtn chn idx

toggleBtns :: LkChn -> SE [Evt D]
toggleBtns chn = mapM (toggleBtn chn) [1 .. 8]

-- pressBtn :: Chn -> Int -> Evt Bool

-- pressBtnSig :: Chn -> Int -> SE Sig
-- toggleBtnSig :: Chn -> Int -> SE Sig
--

----------------------------------------------

upNum, downNum, leftNum, rightNum :: D
upNum = 114
downNum = 115
leftNum = 116
rightNum = 117

arrowUpSig :: LkChn -> SE Sig
arrowUpSig = arrowSig upNum

arrowDownSig :: LkChn -> SE Sig
arrowDownSig = arrowSig downNum

arrowLeftSig :: LkChn -> SE Sig
arrowLeftSig = arrowSig leftNum

arrowRightSig :: LkChn -> SE Sig
arrowRightSig = arrowSig rightNum

arrowSig :: D -> LkChn -> SE Sig
arrowSig idx (LkChn chn) =
  umidiCtrl (int chn) idx 0

------------------------------------------------

arrowUpTap :: LkChn -> SE Tick
arrowUpTap = toArrowTap upNum

arrowDownTap :: LkChn -> SE Tick
arrowDownTap = toArrowTap downNum

arrowLeftTap :: LkChn -> SE Tick
arrowLeftTap = toArrowTap leftNum

arrowRightTap :: LkChn -> SE Tick
arrowRightTap = toArrowTap rightNum

toArrowTap :: D -> LkChn -> SE Tick
toArrowTap idx chn =
  fmap (fmap (const unit) . filterE ((==* 1) . sig) . snaps) $ arrowSig idx chn

------------------------------------------------

arrowUpToggle :: LkChn -> SE (Evt D)
arrowUpToggle = toArrowToggle upNum

arrowDownToggle :: LkChn -> SE (Evt D)
arrowDownToggle = toArrowToggle downNum

arrowLeftToggle :: LkChn -> SE (Evt D)
arrowLeftToggle = toArrowToggle leftNum

arrowRightToggle :: LkChn -> SE (Evt D)
arrowRightToggle = toArrowToggle rightNum

toArrowToggle :: D -> LkChn -> SE (Evt D)
toArrowToggle idx chn = fmap snaps $ arrowSig idx chn
