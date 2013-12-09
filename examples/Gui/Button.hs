module Button where

import Csound.Base
import Data.Tuple

echoInstr :: String -> Unit -> SE ()
echoInstr str _ = prints (text str) []

main = dac $ do
    (gbut, evt) <- toggle "play"
    (gbut2, evt2) <- button "Hi!"
    let evtOn  = filterE (==* 1) evt
        evtOff = filterE (==* 0) evt
    (gvol, vol) <- masterVolume
    let instr _ = return $ (vol * fades 0.5 0.5) * osc 440
    panel $ ver [gvol, gbut, gbut2]
    let res2 = sched (\_ -> return $ mul (fades 0.1 0.1) $ osc 440) $ withDur 0.2 $ fmap (const unit) evt2
    return $ res2 + schedToggle (instr unit) evt
      
-- main = dac $ osc 440
