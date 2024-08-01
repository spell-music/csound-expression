module Csound.Typed.Opcode.SerialIO (
  arduinoRead,
  arduinoReadF,
  arduinoStart,
  arduinoStop,
  serialBegin,
  serialEnd,
  serialFlush,
  serialPrint,
  serialRead,
  serialWrite,
  serialWrite_i,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

--
-- > kval  arduinoRead  iPort, iStream[, iSmooth]
--
-- csound doc: <https://csound.com/docs/manual/arduinoRead.html>
arduinoRead :: D -> D -> Sig
arduinoRead b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "arduinoRead" [(Kr, [Ir, Ir, Ir])] [a1, a2]

--
-- > kval  arduinoReadF  iPort, iStream1,
-- >           iStream2, iStream3
--
-- csound doc: <https://csound.com/docs/manual/arduinoReadF.html>
arduinoReadF :: D -> D -> D -> D -> Sig
arduinoReadF b1 b2 b3 b4 =
  Sig $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4
  where
    f a1 a2 a3 a4 = opcs "arduinoReadF" [(Kr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

--
-- > iPort  arduinoStart  SPortName [, ibaudRate]
--
-- csound doc: <https://csound.com/docs/manual/arduinoStart.html>
arduinoStart :: Str -> D
arduinoStart b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "arduinoStart" [(Ir, [Sr, Ir])] [a1]

--
-- >   arduinoStop  iPort
--
-- csound doc: <https://csound.com/docs/manual/arduinoStop.html>
arduinoStop :: D -> SE ()
arduinoStop b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "arduinoStop" [(Xr, [Ir])] [a1]

{- |
Open a serial port.

Open a serial port for arduino.

> iPort  serialBegin  SPortName [, ibaudRate]

csound doc: <https://csound.com/docs/manual/serialBegin.html>
-}
serialBegin :: Str -> SE D
serialBegin b1 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep "serialBegin" [(Ir, [Sr, Ir])] [a1]

{- |
Close a serial port.

Close a serial port for arduino.

>   serialEnd  iPort

csound doc: <https://csound.com/docs/manual/serialEnd.html>
-}
serialEnd :: D -> SE ()
serialEnd b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "serialEnd" [(Xr, [Ir])] [a1]

{- |
Flush data from a serial port.

Flush to the screen any bytes (up to 32k) in the input buffer.
Note that these bytes will be cleared from the buffer.
use this opcode mainly for debugging messages.
If you want to mix debugging and other communication
messages over the same port, you will need to manually
parse the data with the serialRead opcode.

>   serialFlush  iPort

csound doc: <https://csound.com/docs/manual/serialFlush.html>
-}
serialFlush :: D -> SE ()
serialFlush b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "serialFlush" [(Xr, [Ir])] [a1]

{- |
Print data from a serial port.

Print to the screen any bytes (up to 32k) in the input buffer.
Note that these bytes will be cleared from the buffer.
use this opcode mainly for debugging messages.
If you want to mix debugging and other communication
messages over the same port, you will need to manually
parse the data with the serialRead opcode.

>   serialPrint  iPort

csound doc: <https://csound.com/docs/manual/serialPrint.html>
-}
serialPrint :: D -> SE ()
serialPrint b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "serialPrint" [(Xr, [Ir])] [a1]

{- |
Read data from a serial port.

Read data from a serial port for arduino.

> kByte  serialRead  iPort

csound doc: <https://csound.com/docs/manual/serialRead.html>
-}
serialRead :: D -> Sig
serialRead b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "serialRead" [(Kr, [Ir])] [a1]

{- |
Write data to a serial port.

Write data to a serial port for arduino.

>   serialWrite  iPort, iByte
>   serialWrite  iPort, kByte
>   serialWrite  iPort, SBytes

csound doc: <https://csound.com/docs/manual/serialWrite.html>
-}
serialWrite :: D -> D -> SE ()
serialWrite b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "serialWrite" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Write data to a serial port.

Write data to a serial port for arduino.

>   serialWrite_i  iPort, iByte
>   serialWrite_i  iPort, SBytes

csound doc: <https://csound.com/docs/manual/serialWrite_i.html>
-}
serialWrite_i :: D -> D -> SE ()
serialWrite_i b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "serialWrite_i" [(Xr, [Ir, Ir])] [a1, a2]
