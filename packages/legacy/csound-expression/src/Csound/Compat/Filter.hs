-- | Use this module if your version of Csound is pre 6.09.
module Csound.Compat.Filter(
   -- One pole filters
    zdf1, zlp1, zhp1, zap1,

    -- Two pole filters
    zdf2, zlp, zbp, zhp, zdf2_notch, zbr,

    -- Ladder filter
    zladder,

    -- * TB303 filter
    diode, linDiode, noNormDiode,

    -- Korg 35 filters
    linKorg_lp, linKorg_hp, korg_lp, korg_hp
) where

import Csound.Typed.Plugins(
        zdf1, zlp1, zhp1, zap1,
        zdf2, zlp, zbp, zhp, zdf2_notch, zbr,
        zladder,
        diode, linDiode, noNormDiode,
        linKorg_lp, linKorg_hp, korg_lp, korg_hp)
