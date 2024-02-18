module Csound.Typed.Plugins(
    adsr140,
    audaciousEq,

    -- Solina chorus
    solinaChorus, testSolinaChorus,

    -- One pole filters
    zdf1, zlp1, zhp1, zap1,

    -- Two pole filters
    zdf2, zlp, zbp, zhp, zdf2_notch, zbr,

    -- Ladder filter
    zladder,

    -- Four poles filters
    zdf4, zlp4, zbp4, zhp4,

    -- Eq-filters
    peakEq, highShelf, lowShelf,

    -- Diode ladder filters
    diode, linDiode, noNormDiode,

    -- Korg 35 filters
    linKorg_lp, linKorg_hp, korg_lp, korg_hp,

    -- zero delay convolution
    ZConvSpec(..), zconv, zconv',

    -- ptich shifter delay
    pitchShifterDelay,

    -- Iain's fxs
    fxAnalogDelay, fxDistortion, fxEnvelopeFollower, fxFlanger, fxFreqShifter, fxLoFi,
    fxPanTrem, fxMonoTrem, fxPhaser, fxPitchShifter, fxReverse, fxRingModulator, fxChorus2, fxPingPong,

    -- * Tape echo
    tapeRead, tapeWrite, tapeEcho,

    -- * Live row
    liveRow, liveRows,

    -- * Ambi row
    ambiRow, ambiRowMp3,

    -- utilities
    delay1k

) where

import Csound.Typed.Plugins.Adsr140
import Csound.Typed.Plugins.Zdf
import Csound.Typed.Plugins.Diode
import Csound.Typed.Plugins.Audaciouseq
import Csound.Typed.Plugins.Korg35
import Csound.Typed.Plugins.SolinaChorus
import Csound.Typed.Plugins.ZeroDelayConvolution
import Csound.Typed.Plugins.Iain
import Csound.Typed.Plugins.AmbiRow
import Csound.Typed.Plugins.LiveRow
import Csound.Typed.Plugins.TapeEcho
import Csound.Typed.Plugins.Utilities
