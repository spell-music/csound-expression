module Csound.Typed.Opcode.RemoteOpcodes (
  insglobal,
  insremot,
  midglobal,
  midremot,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

{- |
An opcode which can be used to implement a remote
      orchestra. This opcode will send note events from a source
      machine to many destinations.

With the insremot and
      insglobal opcodes you are able to perform
      instruments on remote machines and control them from a master
      machine. The remote opcodes are implemented using the
      master/client model. All the machines involved contain the same
      orchestra but only the master machine contains the information
      of the score. During the performance the master machine sends
      the note events to the clients. The
      insglobal opcode sends the events to all
      the machines involved in the remote concert. These machines are
      determined by the insremot
      definitions made above the insglobal
      command. To send events to only one machine use insremot.

>  insglobal  isource, instrnum [,instrnum...]

csound doc: <https://csound.com/docs/manual/insglobal.html>
-}
insglobal :: D -> D -> SE ()
insglobal b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "insglobal" [(Xr, (repeat Ir))] [a1, a2]

{- |
An opcode which can be used to implement a remote
      orchestra. This opcode will send note events from a source
      machine to one destination.

With the insremot and insglobal
      opcodes you are able to perform instruments on remote machines
      and control them from a master machine. The remote opcodes are
      implemented using the master/client model. All the machines
      involved contain the same orchestra but only the master machine
      contains the information of the score. During the performance
      the master machine sends the note events to the clients. The
      insremot opcode will send events from a
      source machine to one destination if you want to send events to
      many destinations (broadcast) use the insglobal
      opcode instead. These two opcodes can be used in combination.

>  insremot  idestination, isource, instrnum [,instrnum...]

csound doc: <https://csound.com/docs/manual/insremot.html>
-}
insremot :: D -> D -> D -> SE ()
insremot b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "insremot" [(Xr, (repeat Ir))] [a1, a2, a3]

{- |
An opcode which can be used to implement a remote midi orchestra. This opcode will broadcast the midi events to all the machines involved in the remote concert.

With the midremot and midglobal opcodes you are able to perform instruments on remote machines and control them from a master machine. The remote opcodes are implemented using the master/client model. All the machines involved contain the same orchestra but only the master machine contains the information of the midi score. During the performance the master machine sends the midi events to the clients. The midglobal opcode sends the events to all the machines involved in the remote concert. These machines are determined by the midremot definitions made above the midglobal command. To send events to only one machine use midremot.

>  midglobal  isource, instrnum [,instrnum...]

csound doc: <https://csound.com/docs/manual/midglobal.html>
-}
midglobal :: D -> D -> SE ()
midglobal b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "midglobal" [(Xr, (repeat Ir))] [a1, a2]

{- |
An opcode which can be used to implement a remote midi orchestra. This opcode will send midi events from a source machine to one destination.

With the midremot and midglobal opcodes you are able to perform instruments on remote machines and control them from a master machine. The remote opcodes are implemented using the master/client model. All the machines involved contain the same orchestra but only the master machine contains the information of the midi score. During the performance the master machine sends the midi events to the clients. The midremot opcode will send events from a source machine to one destination if you want to send events to many destinations (broadcast) use the midglobal opcode instead. These two opcodes can be used in combination.

>  midremot  idestination, isource, instrnum [,instrnum...]

csound doc: <https://csound.com/docs/manual/midremot.html>
-}
midremot :: D -> D -> D -> SE ()
midremot b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "midremot" [(Xr, (repeat Ir))] [a1, a2, a3]
