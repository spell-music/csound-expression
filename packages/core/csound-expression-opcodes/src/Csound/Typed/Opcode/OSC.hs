module Csound.Typed.Opcode.OSC (
    
    
    
    oscBundle, oscCount, oscInit, oscInitM, oscRaw, oscSend) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 

--
-- >  OSCbundle  kwhen, ihost, iport,
-- >         Sdest[], Stype[],kArgs[][][,isize]
--
-- csound doc: <https://csound.com/docs/manual/OSCbundle.html>
oscBundle ::  Sig -> D -> D -> Str -> SE ()
oscBundle b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unStr) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "OSCbundle" [(Xr,[Kr,Ir,Ir,Sr,Sr,Kr,Ir])] [a1,a2,a3,a4]

-- | 

--
-- > kans  OSCcount 
--
-- csound doc: <https://csound.com/docs/manual/OSCcount.html>
oscCount ::   Sig
oscCount  =
  Sig $ return $ f 
  where
    f  = opcs "OSCcount" [(Kr,[])] []

-- | 
-- Start a listening process for OSC messages to a particular port.
--
-- Starts a listening process, which can be used by OSClisten.
--
-- > ihandle  OSCinit  iport
--
-- csound doc: <https://csound.com/docs/manual/OSCinit.html>
oscInit ::  D -> SE D
oscInit b1 =
  fmap ( D . return) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep "OSCinit" [(Ir,[Ir])] [a1]

-- | 

--
-- > ihandle  OSCinitM  Sgroup, iport
--
-- csound doc: <https://csound.com/docs/manual/OSCinitM.html>
oscInitM ::  Str -> D -> D
oscInitM b1 b2 =
  D $ f <$> unStr b1 <*> unD b2
  where
    f a1 a2 = opcs "OSCinitM" [(Ir,[Sr,Ir])] [a1,a2]

-- | 
-- Listen for all OSC messages at a given port.
--
-- On each k-cycle looks to see if an OSC message has been received
--       at a given port and copies its contents to a string array. All
--       messages are copied. If a bundle of messages is received, the
--       output array will contain all of the messages in it.
--
-- > Smess[],klen  OSCraw  iport
--
-- csound doc: <https://csound.com/docs/manual/OSCraw.html>
oscRaw :: forall a . Tuple a => D -> a
oscRaw b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "OSCraw" ([Sr,Kr],[Ir]) [a1]

-- | 
-- Sends data to other processes using the OSC protocol
--
-- Uses the OSC protocol to send message to other OSC listening processes.
--
-- >  OSCsend  kwhen, ihost, iport, idestination[, itype , xdata1, xdata2, ...]
--
-- csound doc: <https://csound.com/docs/manual/OSCsend.html>
oscSend ::  Sig -> D -> D -> D -> D -> [Sig] -> SE ()
oscSend b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> mapM (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "OSCsend" [(Xr,[Kr,Ir,Ir,Ir,Ir] ++ (repeat Xr))] ([a1
                                                                                     ,a2
                                                                                     ,a3
                                                                                     ,a4
                                                                                     ,a5] ++ a6)