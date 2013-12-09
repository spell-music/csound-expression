module Waves where

import Csound.Base

import Data.Colour.Names(white, gray, yellow)

main = vdac $ do
    (gvol, vol) <- masterVolume
    (gw, f) <- classicWaves "waves" 0
    let instr x = vol * f x
    panel $ ver [hor [sca 0.1 gvol, gw]]
    return $ midi $ onMsg instr

