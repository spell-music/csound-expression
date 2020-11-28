module Csound.Typed.Opcode.JackoOpcodes (
    
    
    
    jackoAudioIn, jackoAudioInConnect, jackoAudioOut, jackoAudioOutConnect, jackoInit, jackoMidiInConnect, jackoMidiOut, jackoMidiOutConnect, jackoNoteOut, jackoOn, jackoTransport) where

import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

-- 

-- | 
-- Receives an audio signal from a Jack port.
--
-- Receives an audio signal from a Jack audio input port 
--       inside this instance of Csound, which in turn has 
--       received the signal from its connected external Jack 
--       audio output port.
--
-- > asignal  JackoAudioIn ScsoundPortName
--
-- csound doc: <http://csound.com/docs/manual/JackoAudioIn.html>
jackoAudioIn ::  Str -> SE Sig
jackoAudioIn b1 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unStr b1
    where f a1 = opcs "JackoAudioIn" [(Ar,[Sr])] [a1]

-- | 
-- Creates an audio connection from a Jack port to Csound.
--
-- In the orchestra header, creates an audio connection 
--       from an external Jack audio output port to a 
--       Jack audio input port inside this instance of Csound.
--
-- >  JackoAudioInConnect SexternalPortName, ScsoundPortName
--
-- csound doc: <http://csound.com/docs/manual/JackoAudioInConnect.html>
jackoAudioInConnect ::  Str -> Str -> SE ()
jackoAudioInConnect b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "JackoAudioInConnect" [(Xr,[Sr,Sr])] [a1,a2]

-- | 
-- Sends an audio signal to a Jack port.
--
-- Sends an audio signal to an internal Jack audio 
--       output port, and in turn to its connected external 
--       Jack audio input port.
--
-- >  JackoAudioOut  ScsoundPortName, asignal
--
-- csound doc: <http://csound.com/docs/manual/JackoAudioOut.html>
jackoAudioOut ::  Str -> Sig -> SE ()
jackoAudioOut b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2
    where f a1 a2 = opcs "JackoAudioOut" [(Xr,[Sr,Ar])] [a1,a2]

-- | 
-- Creates an audio connection from Csound to a Jack port.
--
-- In the orchestra header, creates an audio connection 
--       from a Jack audio output port inside this instance 
--       of Csound to an external Jack audio input port.
--
-- >  JackoAudioOutConnect ScsoundPortName, SexternalPortName
--
-- csound doc: <http://csound.com/docs/manual/JackoAudioOutConnect.html>
jackoAudioOutConnect ::  Str -> Str -> SE ()
jackoAudioOutConnect b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "JackoAudioOutConnect" [(Xr,[Sr,Sr])] [a1,a2]

-- | 
-- Initializes Csound as a Jack client.
--
-- Initializes this instance of Csound as a Jack client.
--
-- >  JackoInit ServerName, SclientName
--
-- csound doc: <http://csound.com/docs/manual/JackoInit.html>
jackoInit ::  Str -> Str -> SE ()
jackoInit b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "JackoInit" [(Xr,[Sr,Sr])] [a1,a2]

-- | 
-- Creates a MIDI  connection from a Jack port to Csound.
--
-- In the orchestra header, creates a MIDI connection 
--       from an external Jack MIDI output port to this instance of Csound.
--
-- >  JackoMidiInConnect SexternalPortName, ScsoundPortName
--
-- csound doc: <http://csound.com/docs/manual/JackoMidiInConnect.html>
jackoMidiInConnect ::  Str -> Str -> SE ()
jackoMidiInConnect b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "JackoMidiInConnect" [(Xr,[Sr,Sr])] [a1,a2]

-- | 
-- Sends a MIDI channel message to a Jack port.
--
-- Sends a MIDI channel message to a Jack MIDI output port
--       inside this instance of Csound, and in turn to its 
--       connected external Jack MIDI input port.
--
-- >  JackoMidiOut  ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]
--
-- csound doc: <http://csound.com/docs/manual/JackoMidiOut.html>
jackoMidiOut ::  Str -> Sig -> Sig -> Sig -> SE ()
jackoMidiOut b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "JackoMidiOut" [(Xr,[Sr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Creates a MIDI connection from Csound to a Jack port.
--
-- In the orchestra header, creates a connection 
--       from a Jack MIDI output port inside this instance 
--       of Csound to an external Jack MIDI input port.
--
-- >  JackoMidiOutConnect ScsoundPortName, SexternalPortName
--
-- csound doc: <http://csound.com/docs/manual/JackoMidiOutConnect.html>
jackoMidiOutConnect ::  Str -> Str -> SE ()
jackoMidiOutConnect b1 b2 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unStr b2
    where f a1 a2 = opcs "JackoMidiOutConnect" [(Xr,[Sr,Sr])] [a1,a2]

-- | 
-- Sends a MIDI channel message to a Jack port.
--
-- Sends a MIDI channel message to a Jack MIDI output port
--       inside this instance of Csound, and in turn to its 
--       connected external Jack MIDI input port.
--
-- >  JackoNoteOut  ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]
--
-- csound doc: <http://csound.com/docs/manual/JackoNoteOut.html>
jackoNoteOut ::  Str -> Sig -> Sig -> Sig -> SE ()
jackoNoteOut b1 b2 b3 b4 = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> unSig b2 <*> unSig b3 <*> unSig b4
    where f a1 a2 a3 a4 = opcs "JackoNoteOut" [(Xr,[Sr,Kr,Kr,Kr,Kr])] [a1,a2,a3,a4]

-- | 
-- Enables or disables all Jack ports.
--
-- In the orchestra header, after all Jack connections have been created, enables
--       or disables all Jack input and output opcodes 
--       inside this instance of Csound to read or write data.
--
-- >  JackoOn [iactive] 
--
-- csound doc: <http://csound.com/docs/manual/JackoOn.html>
jackoOn ::   SE ()
jackoOn  = SE $ (depT_ =<<) $ lift $ return $ f 
    where f  = opcs "JackoOn" [(Xr,[Ir])] []

-- | 
-- Control the Jack transport.
--
-- Starts, stops, or repositions the Jack transport.
--       This is useful, e.g., for starting an external sequencer
--       playing to send MIDI messages to Csound.
--
-- >  JackoTransport  kcommand, [kposition]
--
-- csound doc: <http://csound.com/docs/manual/JackoTransport.html>
jackoTransport ::  Sig -> SE ()
jackoTransport b1 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1
    where f a1 = opcs "JackoTransport" [(Xr,[Kr,Kr])] [a1]