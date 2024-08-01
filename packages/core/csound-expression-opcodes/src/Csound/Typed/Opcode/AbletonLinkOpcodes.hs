module Csound.Typed.Opcode.AbletonLinkOpcodes (
  link_beat_force,
  link_beat_get,
  link_beat_request,
  link_create,
  ableton_link_enable,
  link_is_enabled,
  link_metro,
  link_peers,
  link_tempo_get,
  link_tempo_set,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

{- |
Forces the global network Ableton Link session to adopt a specific beat number and time.

Forces the global network Ableton Link session to adopt a specific beat number and time, like a conductor stopping an orchestra and immediately starting it again.

>  link_beat_force   i_peer, k_beat [, k_at_time_seconds  [, k_quantum ]]

csound doc: <https://csound.com/docs/manual/link_beat_force.html>
-}
link_beat_force :: D -> Sig -> SE ()
link_beat_force b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "link_beat_force" [(Xr, [Ir, Kr, Kr, Kr])] [a1, a2]

{- |
Returns the beat, phase with respect to the local quantum, and current time for the session.

Returns the beat number, phase of the beat with respect to the local quantum of the beat, and current time for the global network Ableton Link session.

> k_beat_number, k_phase, k_current_time_seconds  link_beat_get  i_peer [, k_quantum]

csound doc: <https://csound.com/docs/manual/link_beat_get.html>
-}
link_beat_get :: D -> (Sig, Sig, Sig)
link_beat_get b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "link_beat_get" ([Kr, Kr, Kr], [Ir, Kr]) [a1]

{- |
Requests the global network Ableton Link session to adopt a specific beat number and time.

>  link_beat_request   i_peer, k_beat [, k_at_time_seconds  [, k_quantum ]]

csound doc: <https://csound.com/docs/manual/link_beat_request.html>
-}
link_beat_request :: D -> Sig -> SE ()
link_beat_request b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "link_beat_request" [(Xr, [Ir, Kr, Kr, Kr])] [a1, a2]

{- |
Creates a peer in an Ableton Link network session.

Creates a peer in an Ableton Link network session. The first peer in a session determines the initial tempo of the session. The value returned must be passed as the first parameter to all subsequent Ableton Link opcode calls for this peer.

> i_peer  link_create  [i_bpm]

csound doc: <https://csound.com/docs/manual/link_create.html>
-}
link_create :: D
link_create =
  D $ return $ f
  where
    f = opcs "link_create" [(Ir, [Ir])] []

{- |
Enable or disable synchronization with the Ableton Link session.

Enable or disable synchronization with the global network Ableton Link session tempo and beat.

>   ableton_link_enable  i_peer [, k_enable]

csound doc: <https://csound.com/docs/manual/link_enable.html>
-}
ableton_link_enable :: D -> SE ()
ableton_link_enable b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "ableton_link_enable" [(Xr, [Ir, Kr])] [a1]

{- |
Returns whether or not this peer is synchronized with the global network Ableton Link session.

Returns whether or not the beat and time of his peer are synchronized with the global network Ableton Link session.

> k_is_enabled  link_is_enabled  i_peer

csound doc: <https://csound.com/docs/manual/link_is_enabled.html>
-}
link_is_enabled :: D -> Sig
link_is_enabled b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "link_is_enabled" [(Kr, [Ir])] [a1]

{- |
Returns a trigger that is 1 on the beat and 0 otherwise along with beat, phase, and time for this session of Ableton Link.

Returns a trigger that is 1 on the beat and 0 otherwise along with the beat, phase, and current time of Ableton Link for this session for a given quantum.

> k_trigger, k_beat, k_phase, k_current_time_seconds  link_metro  i_peer [, k_quantum]

csound doc: <https://csound.com/docs/manual/link_metro.html>
-}
link_metro :: D -> (Sig, Sig, Sig, Sig)
link_metro b1 =
  pureTuple $ f <$> unD b1
  where
    f a1 = mopcs "link_metro" ([Kr, Kr, Kr, Kr], [Ir, Kr]) [a1]

{- |
Returns the number of peers in the session.

Returns the number of peers in the global network Ableton Link session.

> k_count  link_peers  i_peer

csound doc: <https://csound.com/docs/manual/link_peers.html>
-}
link_peers :: D -> Sig
link_peers b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "link_peers" [(Kr, [Ir])] [a1]

{- |
Returns the current tempo of the global network Ableton Link session.

> k_bpm  link_tempo_get  i_peer

csound doc: <https://csound.com/docs/manual/link_tempo_get.html>
-}
link_tempo_get :: D -> Sig
link_tempo_get b1 =
  Sig $ f <$> unD b1
  where
    f a1 = opcs "link_tempo_get" [(Kr, [Ir])] [a1]

{- |
Sets the tempo.

Sets the local tempo if this peer is not enabled; sets the tempo of the global network Ableton Link session if this peer is enabled.

>  link_tempo_set  i_peer, k_bpm [, k_at_time_seconds]

csound doc: <https://csound.com/docs/manual/link_tempo_set.html>
-}
link_tempo_set :: D -> Sig -> SE ()
link_tempo_set b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "link_tempo_set" [(Xr, [Ir, Kr, Kr])] [a1, a2]
