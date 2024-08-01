-- | Simple buttons
module Button where

import Csound.Base

main = appGui

-- | Applicative style guis
appGui = dac $ vlift3 f masterVolume (toggle "play" False) (button "Hi!")
  where
    f vol evt1 evt2 = vol * sum [sig1, sig2]
      where
        (evtOn, evtOff) = splitToggle evt1
        instr cps _ = return $ fades 0.5 0.5 * osc cps
        sig1 = schedUntil (instr 440) evtOn evtOff
        sig2 = sched (instr 330) $ withDur 0.2 $ fmap (const unit) evt2

-- | Monadic style guis
monadGui = dac $ do
  -- Let's create a toggle button with label "play".
  -- The toggle button emmits ones and zeros.
  (gbut1, evt1) <- toggle "play" False

  -- Let's create a plain button with label "Hi!".
  (gbut2, evt2) <- button "Hi!"

  -- It splits the toggle button events on only ones and only zeros.
  let
    (evtOn, evtOff) = splitToggle evt1

  -- Let's use a standard method to create a master volume slider.
  (gvol, vol) <- masterVolume

  -- A simple instrument that plays a single note
  -- it fades in and out in 0.5 seconds.
  let
    instr cps _ = return $ fades 0.5 0.5 * osc cps

  -- Creates a window with our elements vertically aligned.
  panel $ ver [gvol, gbut1, gbut2]

  -- Plays a note while toggle button is 'on'.
  let
    sig1 = schedUntil (instr 440) evtOn evtOff
    -- Plays a note once.
    sig2 = sched (instr 330) $ withDur 0.2 $ fmap (const unit) evt2

  -- Sends the sum to the output scaled with given master volume.
  return $ vol * sum [sig1, sig2]
