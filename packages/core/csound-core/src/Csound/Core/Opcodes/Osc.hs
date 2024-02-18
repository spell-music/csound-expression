-- | OSC protocol
module Csound.Core.Opcodes.Osc
  ( OscHandle (..)
  , oscInit
  , oscListen
  , oscSend
  ) where

import Data.Text qualified as Text
import Csound.Dynamic (E, Var)
import Csound.Dynamic qualified as Dynamic
import Csound.Dynamic (Rate (..))
import Csound.Core.Types
import Csound.Core.State (Dep)
import Control.Monad.Trans.Class (lift)

newtype OscHandle = OscHandle D
  deriving (Val, Tuple, Arg)

-- | Returns OSC-handle.
oscInit :: D -> SE OscHandle
oscInit port = pure $ OscHandle $ readOnlyVar $ liftOpc "OSCinit" [(Ir, [Ir])] port

-- | We allocate references and inline them to OSCInit expression
-- We need to do it because in this opcode Csound mutates the arguments
rawOscListen :: E -> E -> E -> [Var] -> Dep E
rawOscListen oscHandle addr oscType vars =
  Dynamic.opcsDep "OSClisten" [(Kr, Ir:Sr:Sr: (Dynamic.varRate <$> vars))] (oscHandle : addr : oscType : fmap Dynamic.inlineVar vars)

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
oscListen :: forall a . Tuple a => OscHandle -> Str -> Str -> SE (BoolSig, Ref a)
oscListen handle addr typeCode = do
  ref <- initOscRef
  cond <- listen ref
  pure (cond, ref)
  where
    listen :: Tuple a => Ref a -> SE BoolSig
    listen ref = fmap (equals 1) $ csdOscListen ref

    csdOscListen :: Tuple a => Ref a -> SE Sig
    csdOscListen (Ref refVars) = SE $ fmap (Sig . pure) $ do
        expOscHandle <- lift (toE handle)
        expAddr <- lift (toE addr)
        expOscType <- lift (toE typeCode)
        rawOscListen expOscHandle expAddr expOscType refVars

    initOscRef :: SE (Ref a)
    initOscRef = do
      oscRates <- getOscRates typeCode
      SE $ fmap Ref $ Dynamic.newLocalVars oscRates (fromTuple $ (defTuple :: a))

-- | OSCSend - send OSC message
--
-- > oscSend kwhen oscHandle oscAddress typeCode args
--
-- csound docs: <https://csound.com/docs/manual/OSCsend.html>
oscSend :: Tuple a => Sig -> Str -> D -> Str -> Str -> a -> SE ()
oscSend kwhen addr port dest typeCode args = do
  oscRates <- getOscRates typeCode
  liftOpcDep_ "OSCsend" [rates oscRates] (kwhen, addr, port, dest, typeCode, args)
  where
    rates oscRates = (Xr, Kr : Sr: Ir : Sr : Sr : oscRates)

getOscRates :: Str -> SE [Rate]
getOscRates typeCode = do
  typeCodeE <- SE (lift (toE typeCode))
  pure $ case Dynamic.getPrimUnsafe typeCodeE of
    Dynamic.PrimString str -> getOscRate <$> Text.unpack str
    _ -> error "OSCListen: osc type is not a primitive string"
  where
    getOscRate :: Char -> Rate
    getOscRate x = case x of
        'a' -> Ar
        's' -> Sr
        'i' -> Kr
        'f' -> Kr
        _   -> Kr

