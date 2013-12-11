-- | Keyboard events
module Main where

import Csound.Base

main = dac $ do     
    -- Creates a window that listens to keyboard events when in focus.
    keyPanel =<< box "hi"
    
    -- a pure tone instrument that plays a single frequency
    let instr x = const $ return $ osc x
    -- triggers the instrument with frequency on the give event stream
    -- and holds the note wile second stream is silent.
        asig cps evtOn evtOff = schedUntil (instr cps) evtOn evtOff
    
    return $ mean 
        [ asig 330 (charOn 's') (charOff 's') 
        , asig 440 (charOn 'a') (charOff 'a') ]
  
-- Due to inner limitation only one keyboard event can be 
-- registered at one control cycle. So if you press 
-- the two buttons at the same time only one event will be registered.
