module Csound.Typed.Plugins.AmbiRow(
  ambiRow, ambiRowMp3
) where

import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(ambiRowPlugin, ambiRowMp3Plugin)

ambiRow :: Arr1 Str -> Sig -> Sig -> D -> Sig2
ambiRow (Arr sFiles) kSpeed kIndex iFadeTime = toTuple $ fmap ($ 2) $ do
  addUdoPlugin E.ambiRowPlugin
  f sFiles <$> toGE kSpeed <*> toGE kIndex <*> toGE iFadeTime
  where f sFiles kSpeed kIndex iFadeTime = mopcs "AmbiRow" ([Ar, Ar], [Sr, Kr, Kr, Ir]) [inlineVar $ head sFiles, kSpeed, kIndex, iFadeTime]

ambiRowMp3 :: Arr1 Str -> Sig -> Sig -> D -> Sig2
ambiRowMp3 (Arr sFiles) kSpeed kIndex iFadeTime = toTuple $ fmap ($ 2) $ do
  addUdoPlugin E.ambiRowMp3Plugin
  f sFiles <$> toGE kSpeed <*> toGE kIndex <*> toGE iFadeTime
  where f sFiles kSpeed kIndex iFadeTime = mopcs "AmbiRowMp3" ([Ar, Ar], [Sr, Kr, Kr, Ir]) [inlineVar $ head sFiles, kSpeed, kIndex, iFadeTime]

