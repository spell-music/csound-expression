module Csound.Typed.Misc(
    HeadPanSpec(..), headPan, headPan', staticHeadPan
) where

import Data.Default
import Csound.Typed.Types
import Csound.Typed.GlobalState

-- | Static head response based spacialization. It works when you listen in headphones.
-- It works only with sample rate of 44100, 48000 or 96000. It's more efficient than @headPan@.
--
-- > staticHeadPan (azimuth, elevation) asig
--
-- azimuth and elevation are measured in ratios (0, 1),
staticHeadPan :: (D, D) -> Sig -> Sig2
staticHeadPan (az, elev) asrc = toTuple $ do
        azExpr   <- toGE $ -90 + 180 * az
        elevExpr <- toGE $ -40 + 130 * elev
        asrcExpr <- toGE asrc                
        sr       <- toGE $ getSampleRate
        (a1, a2) <- simpleHrtfstat asrcExpr azExpr elevExpr iradius sr
        return [a1, a2]
        where iradius = 9.0

-- | Optional arguments for opcode hrtfmove.
--
-- phase is 0 or 1
--
-- fade is 1 to 24.
--
-- See csound docs for hrtfmove for details.
data HeadPanSpec = HeadPanSpec 
    { headPanPhase :: D
    , hradPanFade  :: D
    }

instance Default HeadPanSpec where
    def = HeadPanSpec 0 8

-- | Head response based spacialization. It works when you listen in headphones.
-- It works only with sample rate of 44100, 48000 or 96000.
--
-- > headPan (azimuth, elevation) asig
--
-- azimuth and elevation are measured in ratios (0, 1),
headPan :: (Sig, Sig) -> Sig -> Sig2
headPan = headPan' def

-- | HeadPan with optional arguments.
headPan' :: HeadPanSpec -> (Sig, Sig) -> Sig -> Sig2
headPan' spec (az, elev) asrc = toTuple $ do
        azExpr   <- toGE $ -90 + 180 * az
        elevExpr <- toGE $ -40 + 130 * elev
        asrcExpr <- toGE asrc        
        phase    <- toGE $ headPanPhase spec
        fade     <- toGE $ hradPanFade  spec
        sr       <- toGE $ getSampleRate
        (a1, a2) <- simpleHrtfmove asrcExpr azExpr elevExpr phase fade sr
        return [a1, a2]