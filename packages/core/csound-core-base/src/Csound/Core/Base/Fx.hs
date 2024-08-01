{-# LANGUAGE TemplateHaskell #-}

{- | Note that if you take ths as example of using UDOs
use versions without "Internal". Because code for internal UDOs
ships with the package but you have to provide the code for UDO on the disk (full path)
to use liftUdo or liftMultiUdo and alike.
-}
module Csound.Core.Base.Fx (
  analogDelay,
  pingPongDelay,
) where

import Csound.Core.Render.Options
import Csound.Core.Types
import Data.FileEmbed (embedFileRelative)
import Data.Text.Encoding qualified as Text
import System.FilePath ((</>))

{- | Analog delay

opcode	AnalogDelay,a,aKKKKi
ain,kmix,ktime,kfback,ktone, iMaxDelayTime	xin			;READ IN INPUT ARGUMENTS
-}
analogDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
analogDelay ain kmix ktime kfback ktone iMaxDelayTime =
  liftOpc "AnalogDelay" rates (withUdo (ain, kmix, ktime, kfback, ktone, iMaxDelayTime))
  where
    rates = [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir])]

    withUdo =
      withOption $
        addUdo
          "AnalogDelay"
          (UdoBody $ Text.decodeUtf8 $ $(embedFileRelative ("data" </> "opcodes" </> "MultiFX" </> "AnalogDelay.udo")))

{- | Stereo ping-pong delay

opcode StereoPingPongDelay, aa, aaKKKKKi
   aInL, aInR, kdelayTime, kFeedback, kMix, kWidth, kDamp, iMaxDelayTime xin
-}
pingPongDelay :: Sig2 -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig2
pingPongDelay (ain1, ain2) kdelayTime kFeedback kMix kWidth kDamp iMaxDelayTime =
  liftMulti
    "StereoPingPongDelay"
    rates
    (withUdo (ain1, ain2, kdelayTime, kFeedback, kMix, kWidth, kDamp, iMaxDelayTime))
  where
    rates = ([Ar, Ar], [Ar, Ar, Kr, Kr, Kr, Kr, Kr, Ir])

    withUdo = withOption (addUdo "StereoPingPongDelay" (UdoBody udoContent))

    udoContent = Text.decodeUtf8 $ $(embedFileRelative ("data" </> "opcodes" </> "MultiFX" </> "StereoPingPongDelay.udo"))
