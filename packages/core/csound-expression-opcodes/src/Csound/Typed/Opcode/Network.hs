module Csound.Typed.Opcode.Network (
    
    
    
    remoteport, sockrecv, sockrecvs, strecv, socksend, socksends, stsend) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Defines the port for use with the remote system.
--
-- Defines the port for use with the insremot, 
--       midremot, insglobal
--       and midglobal opcodes.
--
-- >  remoteport  iportnum
--
-- csound doc: <https://csound.com/docs/manual/remoteport.html>
remoteport ::  D -> SE ()
remoteport b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "remoteport" [(Xr,[Ir])] [a1]

-- | 
-- Receives data from other processes using the low-level UDP or TCP protocols
--
-- Receives directly using the UDP (sockrecv and
--       sockrecvs) or TCP (strecv)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The sockrecvs opcode receives a stereo signal
--       interleaved.
--
-- > asig  sockrecv  iport, ilength
-- > ksig  sockrecv  iport, ilength
--
-- csound doc: <https://csound.com/docs/manual/sockrecv.html>
sockrecv ::  D -> D -> Sig
sockrecv b1 b2 =
  Sig $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = opcs "sockrecv" [(Ar,[Ir,Ir]),(Kr,[Ir,Ir])] [a1,a2]

-- | 
-- Receives data from other processes using the low-level UDP or TCP protocols
--
-- Receives directly using the UDP (sockrecv and
--       sockrecvs) or TCP (strecv)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The sockrecvs opcode receives a stereo signal
--       interleaved.
--
-- > asigl, asigr  sockrecvs  iport, ilength
--
-- csound doc: <https://csound.com/docs/manual/sockrecv.html>
sockrecvs ::  D -> D -> (Sig,Sig)
sockrecvs b1 b2 =
  pureTuple $ f <$> unD b1 <*> unD b2
  where
    f a1 a2 = mopcs "sockrecvs" ([Ar,Ar],[Ir,Ir]) [a1,a2]

-- | 
-- Receives data from other processes using the low-level UDP or TCP protocols
--
-- Receives directly using the UDP (sockrecv and
--       sockrecvs) or TCP (strecv)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The sockrecvs opcode receives a stereo signal
--       interleaved.
--
-- > asig[,kstate]  strecv  Sipaddr, iport
--
-- csound doc: <https://csound.com/docs/manual/sockrecv.html>
strecv :: forall a . Tuple a => Str -> D -> a
strecv b1 b2 =
  pureTuple $ f <$> unStr b1 <*> unD b2
  where
    f a1 a2 = mopcs "strecv" ([Ar,Kr],[Sr,Ir]) [a1,a2]

-- | 
-- Sends data to other processes using the low-level UDP or TCP protocols
--
-- Transmits data directly using the UDP (socksend and
--       socksends) or TCP (stsend)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The socksends opcode send a stereo signal interleaved.
--
-- >  socksend  asig, Sipaddr, iport, ilength
-- >  socksend  ksig, Sipaddr, iport, ilength
--
-- csound doc: <https://csound.com/docs/manual/socksend.html>
socksend ::  Sig -> Str -> D -> D -> SE ()
socksend b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "socksend" [(Xr,[Ar,Sr,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sends data to other processes using the low-level UDP or TCP protocols
--
-- Transmits data directly using the UDP (socksend and
--       socksends) or TCP (stsend)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The socksends opcode send a stereo signal interleaved.
--
-- >  socksends  asigl, asigr, Sipaddr, iport,
-- >         ilength
--
-- csound doc: <https://csound.com/docs/manual/socksend.html>
socksends ::  Sig -> Sig -> Str -> D -> D -> SE ()
socksends b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unStr) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "socksends" [(Xr,[Ar,Ar,Sr,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | 
-- Sends data to other processes using the low-level UDP or TCP protocols
--
-- Transmits data directly using the UDP (socksend and
--       socksends) or TCP (stsend)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The socksends opcode send a stereo signal interleaved.
--
-- >  stsend  asig, Sipaddr, iport
--
-- csound doc: <https://csound.com/docs/manual/socksend.html>
stsend ::  Sig -> Str -> D -> SE ()
stsend b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unStr) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "stsend" [(Xr,[Ar,Sr,Ir])] [a1,a2,a3]