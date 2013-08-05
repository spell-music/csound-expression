-- | Realtime Interaction
module Csound.Opcode.Interaction (
    -----------------------------------------------------
    -- * Human Interfaces

    -- ** Keys
    sensekey,

    -- ** Mouse
    xyin    
) where

import Csound.Exp.Wrapper
import Csound.LowLevel

-----------------------------------------------------
-- * Open Sound Control and Network

-- ** Open Sound Control

-- ** Remote Instruments

-- ** Network Audio

-----------------------------------------------------
-- * Human Interfaces

-- ** Widgets

-- ** Keys

-- | Returns the ASCII code of a key that has been pressed, or -1 if no key has been pressed. 
--
-- > kres[, kkeydown] sensekey
--
-- doc: <http://www.csounds.com/manual/html/sensekey.html>
sensekey :: (Sig, Sig)
sensekey = mopc0 "sensekey" ([k,k], [])

-- ** Mouse

-- | Sense the cursor position in an output window. When xyin is called the position of the mouse within 
-- the output window is used to reply to the request. This simple mechanism does mean that only one xyin 
-- can be used accurately at once. The position of the mouse is reported in the output window. 
--
-- > kx, ky xyin iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]
--
-- doc: <http://www.csounds.com/manual/html/xyin.html>
xyin :: D -> D -> D -> D -> D -> (Sig, Sig)
xyin = mopc5 "xyin" ([k,k], is 7)

-- ** WII

-- ** P5 Glove

