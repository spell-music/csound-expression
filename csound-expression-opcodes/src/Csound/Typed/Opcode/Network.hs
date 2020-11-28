module Csound.Typed.Opcode.Network (
    
    
    
    remoteport, sockrecv, sockrecvs, sockrecv, strecv, socksend, socksends, stsend) where

import Control.Applicative
import Control.Monad.Trans.Class
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
-- csound doc: <http://csound.com/docs/manual/remoteport.html>
remoteport ::  D -> SE ()
remoteport b1 = SE $ (depT_ =<<) $ lift $ f <$> unD b1
    where f a1 = opcs "remoteport" [(Xr,[Ir])] [a1]

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
-- csound doc: <http://csound.com/docs/manual/sockrecv.html>
sockrecv ::  D -> D -> Sig
sockrecv b1 b2 = Sig $ f <$> unD b1 <*> unD b2
    where f a1 a2 = opcs "sockrecv" [(Ar,[Ir,Ir]),(Kr,[Ir,Ir])] [a1,a2]

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
-- csound doc: <http://csound.com/docs/manual/sockrecv.html>
sockrecvs ::  D -> D -> (Sig,Sig)
sockrecvs b1 b2 = pureTuple $ f <$> unD b1 <*> unD b2
    where f a1 a2 = mopcs "sockrecvs" ([Ar,Ar],[Ir,Ir]) [a1,a2]

-- | 
-- Receives data from other processes using the low-level UDP or TCP protocols
--
-- Receives directly using the UDP (sockrecv and
--       sockrecvs) or TCP (strecv)
--       protocol onto a network. The data is not subject to any encoding or special
--       routing. The sockrecvs opcode receives a stereo signal
--       interleaved.
--
-- > asig  strecv  Sipaddr, iport
--
-- csound doc: <http://csound.com/docs/manual/sockrecv.html>
strecv ::  Str -> D -> Sig
strecv b1 b2 = Sig $ f <$> unStr b1 <*> unD b2
    where f a1 a2 = opcs "strecv" [(Ar,[Sr,Ir])] [a1,a2]

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
-- csound doc: <http://csound.com/docs/manual/socksend.html>
socksend ::  Sig -> Str -> D -> D -> SE ()
socksend b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unStr b2 <*> unD b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "socksend" [(Xr,[Ar,Sr,Ir,Ir])] [a1,a2,a3,a4]

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
-- csound doc: <http://csound.com/docs/manual/socksend.html>
socksends ::  Sig -> Sig -> Str -> D -> D -> SE ()
socksends b1 b2 b3 b4 b5 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unStr b3 <*> unD b4 <*> unD b5
    where f a1 a2 a3 a4 a5 = opcs "socksends" [(Xr,[Ar,Ar,Sr,Ir,Ir])] [a1,a2,a3,a4,a5]

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
-- csound doc: <http://csound.com/docs/manual/socksend.html>
stsend ::  Sig -> Str -> D -> SE ()
stsend b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unStr b2 <*> unD b3
    where f a1 a2 a3 = opcs "stsend" [(Xr,[Ar,Sr,Ir])] [a1,a2,a3]