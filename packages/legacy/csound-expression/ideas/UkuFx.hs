import Csound.Base

main = run proc

run = dacBy (setAdc <> setJack "fx" <> setRates 44100 32 <> setBufs 64 32)

proc :: Sig2 -> Source Sig2
proc (a1, a2) = fxApply fx a1
  where
    fx = fxGridMS 4 [uiTort1m, uiFlan1, uiPhasy2, uiAdele2 0.4 0.35] def [uiChory2, uiHall2, uiGain 0.6]
