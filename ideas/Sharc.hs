
import qualified Sharc.Types as Sh
import Csound.Base
import Sharc.Data
import Sharc.Types

note2sig :: Sh.Note -> Sig
note2sig n = oscBy (harmonics2tab $ Sh.noteHarmonics n) (sig $ double $ Sh.pitchFund $ Sh.notePitch n)

note2tab :: Sh.Note -> Tab
note2tab n = (harmonics2tab $ Sh.noteHarmonics n)

deg x = 180 * x / pi

harmonics2tab harmonics = sines3 $ fmap (\h -> (fromIntegral $ Sh.harmonicId h, Sh.harmonicAmplitude h, deg $ Sh.harmonicPhase h)) harmonics


