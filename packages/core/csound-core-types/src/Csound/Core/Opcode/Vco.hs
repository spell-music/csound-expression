module Csound.Core.Opcode.Vco (
  VcoTab (..),
  VcoShape (..),
  VcoInit (..),
  vcoInit,
  vco2ft,
  saw,
  sqr,
  tri,
  pulse,
  isaw,
  blosc,
  saw',
  sqr',
  tri',
  pulse',
  isaw',
  blosc',
  vcoTab,
  oscilikt,
) where

import Csound.Core.State (Run)
import Csound.Core.State qualified as State
import Csound.Core.Types
import Csound.Dynamic (E, Gen, Rate (..))

newtype VcoTab = VcoTab {unVcoTab :: Run E}

instance Val VcoTab where
  fromE = VcoTab
  toE = unVcoTab
  valRate = Ir

instance FromTuple VcoTab where
  fromTuple (VcoTab val) = fmap pure val

instance Tuple VcoTab where
  tupleArity = 1
  tupleRates = tupleRates @Tab
  defTuple = fromE (pure (-1))
  toTuple = fromE . fmap head

data VcoShape = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Tab

data VcoInit = VcoInit
  { vcoShape :: VcoShape
  , vcoMul :: Maybe Double
  , vcoMinSize :: Maybe Int
  , vcoMaxSize :: Maybe Int
  }

toInternalVcoShape :: VcoShape -> Run State.VcoShape
toInternalVcoShape = \case
  Saw -> pure State.Saw
  Pulse -> pure State.Pulse
  Square -> pure State.Square
  Triangle -> pure State.Triangle
  IntegratedSaw -> pure State.IntegratedSaw
  UserGen t -> State.UserGen <$> tab2gen "VCO tab should be primitive" t

toInternalVcoInit :: VcoInit -> Run State.VcoInit
toInternalVcoInit inits = do
  shape <- toInternalVcoShape (vcoShape inits)
  pure $ State.VcoInit shape (vcoMul inits) (vcoMinSize inits) (vcoMaxSize inits)

vcoInit :: VcoInit -> VcoTab
vcoInit inits = VcoTab $ State.saveVco =<< toInternalVcoInit inits

vco2ft :: Sig -> VcoTab -> Tab
vco2ft kcps t = liftOpc "vco2ft" [(Kr, [Kr, Ir, Ir])] (kcps, t)

saw :: Sig -> Sig
saw cps = noPhaseWave Saw cps

isaw :: Sig -> Sig
isaw cps = noPhaseWave IntegratedSaw cps

tri :: Sig -> Sig
tri cps = noPhaseWave Triangle cps

sqr :: Sig -> Sig
sqr cps = noPhaseWave Square cps

pulse :: Sig -> Sig
pulse cps = noPhaseWave Pulse cps

blosc :: Tab -> Sig -> Sig
blosc tab cps = noPhaseWave (UserGen tab) cps

saw' :: D -> Sig -> Sig
saw' phase cps = wave Saw phase cps

isaw' :: D -> Sig -> Sig
isaw' phase cps = wave IntegratedSaw phase cps

tri' :: D -> Sig -> Sig
tri' phase cps = wave Triangle phase cps

sqr' :: D -> Sig -> Sig
sqr' phase cps = wave Square phase cps

pulse' :: D -> Sig -> Sig
pulse' phase cps = wave Pulse phase cps

blosc' :: Tab -> D -> Sig -> Sig
blosc' tab phase cps = wave (UserGen tab) phase cps

noPhaseWave :: VcoShape -> Sig -> Sig
noPhaseWave shape cps = wave shape 0 cps

wave :: VcoShape -> D -> Sig -> Sig
wave shape phase cps =
  oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit shape Nothing Nothing Nothing)) phase

tab2gen :: String -> Tab -> Run Gen
tab2gen msg t = fromPreTab $ getPreTabUnsafe msg t

vcoTab :: Tab -> Sig -> Sig
vcoTab t cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit (UserGen t) Nothing Nothing Nothing)) 0

-- ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
-- kres oscilikt kamp, kcps, kfn [, iphs] [, istor]
oscilikt :: Sig -> Sig -> Tab -> D -> Sig
oscilikt amp cps fn mphase = liftOpc "oscilikt" rates (amp, cps, fn, mphase)
  where
    rates = [(Ar, [Xr, Xr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])]
