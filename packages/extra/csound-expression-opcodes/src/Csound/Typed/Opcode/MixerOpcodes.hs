module Csound.Typed.Opcode.MixerOpcodes (
    
    
    
    mixerClear, mixerGetLevel, mixerReceive, mixerSend, mixerSetLevel, mixerSetLevel_i) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Resets all channels of a buss to 0.
--
-- >  MixerClear 
--
-- csound doc: <https://csound.com/docs/manual/MixerClear.html>
mixerClear ::   SE ()
mixerClear  =
  SE $ join $ return $ f 
  where
    f  = opcsDep_ "MixerClear" [(Xr,[])] []

-- | 
-- Gets the level of a send to a buss.
--
-- Gets the level at which signals from the send are being added to the buss. The actual sending of the signal to the buss 
-- is performed by the MixerSend opcode.
--
-- > kgain  MixerGetLevel  isend, ibuss
--
-- csound doc: <https://csound.com/docs/manual/MixerGetLevel.html>
mixerGetLevel ::  D -> D -> SE Sig
mixerGetLevel b1 b2 =
  fmap ( Sig . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep "MixerGetLevel" [(Kr,[Ir,Ir])] [a1,a2]

-- | 
-- Receives an arate signal from a channel of a buss.
--
-- Receives an arate signal that has been mixed onto a channel of a buss.
--
-- > asignal  MixerReceive  ibuss, ichannel
--
-- csound doc: <https://csound.com/docs/manual/MixerReceive.html>
mixerReceive ::  D -> D -> SE Sig
mixerReceive b1 b2 =
  fmap ( Sig . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep "MixerReceive" [(Ar,[Ir,Ir])] [a1,a2]

-- | 
-- Mixes an arate signal into a channel of a buss.
--
-- >  MixerSend  asignal, isend, ibuss, ichannel
--
-- csound doc: <https://csound.com/docs/manual/MixerSend.html>
mixerSend ::  Sig -> D -> D -> D -> SE ()
mixerSend b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "MixerSend" [(Xr,[Ar,Ir,Ir,Ir])] [a1,a2,a3,a4]

-- | 
-- Sets the level of a send to a buss.
--
-- Sets the level at which signals from the send are added to the buss. The actual sending of the signal to the buss 
-- is performed by the MixerSend opcode.
--
-- >  MixerSetLevel  isend, ibuss, kgain
--
-- csound doc: <https://csound.com/docs/manual/MixerSetLevel.html>
mixerSetLevel ::  D -> D -> Sig -> SE ()
mixerSetLevel b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unSig) b3
  where
    f a1 a2 a3 = opcsDep_ "MixerSetLevel" [(Xr,[Ir,Ir,Kr])] [a1,a2,a3]

-- | 
-- Sets the level of a send to a buss.
--
-- Sets the level at which signals from the send are added to the buss. This opcode, because all parameters are irate, may 
--       be used in the orchestra header. The actual sending of the signal to the buss 
--       is performed by the MixerSend opcode.
--
-- >  MixerSetLevel_i  isend, ibuss, igain
--
-- csound doc: <https://csound.com/docs/manual/MixerSetLevel_i.html>
mixerSetLevel_i ::  D -> D -> D -> SE ()
mixerSetLevel_i b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "MixerSetLevel_i" [(Xr,[Ir,Ir,Ir])] [a1,a2,a3]