{-# Language AllowAmbiguousTypes #-}
-- | OSC protocol
module Csound.Core.Base.Osc
  ( OscHandle (..)
  , oscInit
  , oscListen
  , oscSend
  ) where

import Csound.Core.Types
import Data.String

newtype OscHandle = OscHandle D
  deriving newtype (Val, FromTuple, Tuple, Arg)

-- | Returns OSC-handle.
oscInit :: D -> SE OscHandle
oscInit port = pure $ OscHandle $ readOnlyVar $ liftOpc "OSCinit" [(Ir, [Ir])] port

-- | Listens for the OSC-messages. The first argument is OSC-reference.
-- We can create it with the function @initOsc@. The next two arguments are strings.
-- The former specifies the path-like address to listen the messages. It can be:
--
-- > /foo/bar/baz
--
-- The latter specifies the type of expected arguments.
-- The string can contain the characters "bcdfilmst" which stand for
-- Boolean, character, double, float, 32-bit integer, 64-bit integer, MIDI,
-- string and timestamp.
--
-- The result is boolean signal that indicates when new message is received
-- and reference to mutable variables from which we can read the values
oscListen :: forall a . Tuple a => OscHandle -> Str -> Ref a -> SE BoolSig
oscListen oscHandle addr vars = do
  liftOpcDep "OSClisten" [(Kr, Ir:Sr:Sr: tupleRates @a)] (oscHandle, addr, getOscType @a, vars)

-- | OSCSend - send OSC message
--
-- > oscSend kwhen oscHandle oscAddress args
--
-- csound docs: <https://csound.com/docs/manual/OSCsend.html>
oscSend :: forall a . Tuple a => Sig -> Str -> D -> Str -> a -> SE ()
oscSend kwhen addr port dest args =
  liftOpcDep_ "OSCsend" [rates] (kwhen, addr, port, dest, getOscType @a, args)
  where
    rates = (Xr, Kr : Sr: Ir : Sr : Sr : oscRates)
    oscRates = tupleRates @a

getOscType :: forall a . Tuple a => Str
getOscType =
  fromString $ fmap toCodeChar oscRates
  where
    oscRates = tupleRates @a

    toCodeChar :: Rate -> Char
    toCodeChar = \case
        Ar -> 'a'
        Sr -> 's'
        Kr -> 'd'
        _ -> 'd'
