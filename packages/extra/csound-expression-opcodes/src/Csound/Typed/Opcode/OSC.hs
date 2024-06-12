module Csound.Typed.Opcode.OSC (
    
    
    
    oscInit, oscListen, oscRaw, oscSend) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Start a listening process for OSC messages to a particular port.
--
-- Starts a listening process, which can be used by OSClisten.
--
-- > ihandle  OSCinit  iport
--
-- csound doc: <http://csound.com/docs/manual/OSCinit.html>
oscInit ::  D -> SE D
oscInit b1 =
  fmap ( D . return) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep "OSCinit" [(Ir,[Ir])] [a1]

-- | 
-- Listen for OSC messages to a particular path.
--
-- On each k-cycle looks to see if an OSC message has been send to
--       a given path of a given type.
--
-- > kans  OSClisten  ihandle, idest, itype [, xdata1, xdata2, ...]
--
-- csound doc: <http://csound.com/docs/manual/OSClisten.html>
oscListen ::  D -> D -> D -> [Sig] -> SE Sig
oscListen b1 b2 b3 b4 =
  fmap ( Sig . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unSig) b4
  where
    f a1 a2 a3 a4 = opcsDep "OSClisten" [(Kr,[Ir,Ir,Ir] ++ (repeat Xr))] ([a1,a2,a3] ++ a4)

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
-- csound doc: <http://csound.com/docs/manual/OSCraw.html>
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
-- csound doc: <http://csound.com/docs/manual/OSCsend.html>
oscSend ::  Sig -> D -> D -> D -> D -> [Sig] -> SE ()
oscSend b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> mapM (lift . unSig) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "OSCsend" [(Xr,[Kr,Ir,Ir,Ir,Ir] ++ (repeat Xr))] ([a1
                                                                                     ,a2
                                                                                     ,a3
                                                                                     ,a4
                                                                                     ,a5] ++ a6)