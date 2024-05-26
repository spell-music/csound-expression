-- | Note that if you take ths as example of using UDOs
-- use versions without "Internal". Because code for internal UDOs
-- ships with the package but you have to provide the code for UDO on the disk (full path)
-- to use liftUdo or liftMultiUdo and alike.
module Csound.Core.Opcodes.Fx
  ( analogDelay
  , pingPongDelay
  ) where

import System.FilePath ((</>))
import Csound.Core.Types
import Csound.Dynamic (Rate (..))

-- | Analog delay
--
-- opcode	AnalogDelay,a,aKKKK
-- ain,kmix,ktime,kfback,ktone	xin			;READ IN INPUT ARGUMENTS
analogDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
analogDelay ain kmix ktime kfback ktone =
  liftInternalUdo ("MultiFX" </> "AnalogDelay.udo") "AnalogDelay" rates (ain,kmix,ktime,kfback,ktone)
  where
    rates = [(Ar, [Ar,Kr,Kr,Kr,Kr])]

-- | Stereo ping-pong delay
--
-- opcode StereoPingPongDelay, aa, aaKKKKKi
--    aInL, aInR, kdelayTime, kFeedback, kMix, kWidth, kDamp, iMaxDelayTime xin
pingPongDelay :: Sig2 -> Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig2
pingPongDelay (ain1, ain2) kdelayTime kFeedback kMix kWidth kDamp iMaxDelayTime =
  liftInternalMultiUdo ("MultiFX" </> "StereoPingPongDelay.udo") "StereoPingPongDelay" rates (ain1, ain2, kdelayTime, kFeedback, kMix, kWidth, kDamp, iMaxDelayTime)
  where
    rates = ([Ar,Ar], [Ar,Ar,Kr,Kr,Kr,Kr,Kr,Ir])
