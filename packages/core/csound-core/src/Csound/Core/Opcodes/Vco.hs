module Csound.Core.Opcodes.Vco
  ( VcoTab (..)
  , VcoShape (..)
  , VcoInit (..)
  , vcoInit
  , vco2ft
  , saw
  , sqr
  , tri
  , vcoTab
  , oscilikt
  ) where

import Csound.Dynamic (E, Gen)
import Csound.Dynamic (Rate (..))
import Csound.Core.Types
import Csound.Core.State (Run)
import Csound.Core.State qualified as State

newtype VcoTab = VcoTab { unVcoTab :: Run E }

instance Val VcoTab where
  fromE = VcoTab
  toE = unVcoTab
  valRate = Ir

instance Tuple VcoTab where
  tupleMethods = primTuple (fromE $ pure (-1))

data VcoShape = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Tab

data VcoInit = VcoInit
  { vcoShape   :: VcoShape
  , vcoMul     :: Maybe Double
  , vcoMinSize :: Maybe Int
  , vcoMaxSize :: Maybe Int
  }

toInternalVcoShape :: VcoShape -> Run State.VcoShape
toInternalVcoShape = \case
  Saw   -> pure State.Saw
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
saw cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Saw Nothing Nothing Nothing)) 0

tri :: Sig -> Sig
tri cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Triangle Nothing Nothing Nothing)) 0

sqr :: Sig -> Sig
sqr cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit Square Nothing Nothing Nothing)) 0

tab2gen :: String -> Tab -> Run Gen
tab2gen msg t = fromPreTab $ getPreTabUnsafe msg t

vcoTab :: Tab -> Sig -> Sig
vcoTab t cps = oscilikt 1 cps (vco2ft cps $ vcoInit (VcoInit (UserGen t) Nothing Nothing Nothing)) 0

-- ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
-- kres oscilikt kamp, kcps, kfn [, iphs] [, istor]
oscilikt :: Sig -> Sig -> Tab -> Sig -> Sig
oscilikt amp cps fn mphase = liftOpc "oscilikt" rates (amp, cps, fn, mphase)
  where
    rates = [ (Ar, [Xr, Xr, Kr, Ir, Ir]), (Kr, [Kr, Kr, Kr, Ir, Ir])]

