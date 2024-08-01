module Csound.Typed.Plugins.LiveRow (
  liveRow,
  liveRows,
) where

import Csound.Dynamic

import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Elements qualified as E (liveRowPlugin, liveRowsPlugin)
import Csound.Typed.Types

liveRow :: D -> TabList -> D -> D -> Sig -> Tab -> Sig
liveRow iTabSize iTabs iBpm iBeatDur kUserIndex iAuxParams = fromGE $ do
  addUdoPlugin E.liveRowPlugin
  f <$> toGE iTabSize <*> toGE iTabs <*> toGE iBpm <*> toGE iBeatDur <*> toGE kUserIndex <*> toGE iAuxParams
  where
    f iTabSize' iTabs' iBpm' iBeatDur' kUserIndex' iAuxParams' = opcs "liveRow" [(Ar, [Ir, Ir, Ir, Ir, Kr, Ir])] [iTabSize', iTabs', iBpm', iBeatDur', kUserIndex', iAuxParams']

liveRows :: D -> TabList -> TabList -> D -> D -> Sig -> Tab -> Sig2
liveRows iTabSize iLeftTabs iRightTabs iBpm iBeatDur kUserIndex iAuxParams = toTuple $ fmap ($ 2) $ do
  addUdoPlugin E.liveRowsPlugin
  f <$> toGE iTabSize <*> toGE iLeftTabs <*> toGE iRightTabs <*> toGE iBpm <*> toGE iBeatDur <*> toGE kUserIndex <*> toGE iAuxParams
  where
    f iTabSize' iLeftTabs' iRightTabs' iBpm' iBeatDur' kUserIndex' iAuxParams' = mopcs "liveRows" ([Ar, Ar], [Ir, Ir, Ir, Ir, Ir, Kr, Ir]) [iTabSize', iLeftTabs', iRightTabs', iBpm', iBeatDur', kUserIndex', iAuxParams']
