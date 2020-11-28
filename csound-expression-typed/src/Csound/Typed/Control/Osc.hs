-- | Open sound control
{-# Language ScopedTypeVariables #-}
module Csound.Typed.Control.Osc(
    OscRef, OscHost, OscPort, OscAddress, OscType,
    initOsc, listenOsc, sendOsc,
    OscVal, listenOscVal
) where

import Data.Boolean (ifB, (==*))
import Csound.Dynamic(Rate(..))

import Csound.Typed.Types
import Csound.Typed.GlobalState hiding (oscInit, oscListen, oscSend)
import qualified Csound.Typed.GlobalState as C(oscInit, oscListen, oscSend)

import Csound.Typed.Control.Ref


newtype OscRef = OscRef { unOscRef :: D }

-- | Port to listen OSC-messages.
type OscPort = Int

-- | Path-like string ("/foo/bar/baz")
type OscAddress = String


-- | The string specifies the type of expected arguments.
-- The string can contain the characters "bcdfilmst" which stand for
-- Boolean, character, double, float, 32-bit integer, 64-bit integer, MIDI,
-- string and timestamp.
type OscType = String

-- | The hostname of the computer. An empty string is for local machine.
type OscHost = String

-- | Initializes host client. The process starts to run in the background.
initOsc :: OscPort -> OscRef
initOsc port = OscRef $ fromGE $ getOscPortHandle port

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
-- The result is an event of messages. We can run a callback on it
-- with standard function @runEvt@:
--
-- > runEvt :: Evt a -> (a -> SE ()) -> SE ()
listenOsc :: forall a . Tuple a => OscRef -> OscAddress -> OscType -> Evt a
listenOsc oscRef oscAddr oscType = Evt $ \bam -> do
    resRef <- initOscRef oscType
    cond <- listen resRef
    when1 cond $ bam =<< readRef resRef
    where
        listen :: Tuple a => Ref a -> SE BoolSig
        listen ref = fmap (==* 1) $ csdOscListen ref oscRef oscAddr oscType

        csdOscListen :: Tuple a => Ref a -> OscRef -> OscAddress -> OscType -> SE Sig
        csdOscListen (Ref refVars) oscHandle addr ty =
            fmap fromGE $ fromDep $ hideGEinDep $ do
                expOscHandle <- toGE $ unOscRef oscHandle
                expAddr <- toGE $ text addr
                expOscType <- toGE $ text ty
                return $ C.oscListen expOscHandle expAddr expOscType refVars

        initOscRef :: OscType -> SE (Ref a)
        initOscRef typeStr = fmap Ref $ newLocalVars (fmap getOscRate typeStr) (fromTuple $ (defTuple :: a))

        getOscRate :: Char -> Rate
        getOscRate x = case x of
            'a' -> Ar
            's' -> Sr
            'i' -> Kr
            'f' -> Kr
            _   -> Kr

-- | Sends OSC-messages. It takes in a name of the host computer
-- (empty string is alocal machine), port on which the target
-- machine is listening, OSC-addres and type. The last argument
-- produces the values for OSC-messages.
sendOsc :: forall a . Tuple a => OscHost -> OscPort -> OscAddress -> OscType -> Evt a -> SE ()
sendOsc host port addr ty evts = do
    flagRef <- newRef (0 :: Sig)
    valRef  <- newRef defTuple
    runEvt evts $ \a -> do
        flag <- readRef flagRef
        writeRef flagRef (flag + 1)
        writeRef valRef  a

    flag <- readRef flagRef
    value <- readRef valRef
    send flag value
    where
        send :: Tuple a => Sig -> a -> SE ()
        send trig as = SE $ hideGEinDep $ do
            args <- fromTuple as
            expTrig <- toGE trig
            expHost <- toGE $ text $ host
            expPort <- toGE $ int  $ port
            expAddr <- toGE $ text $ addr
            expTy   <- toGE $ text $ ty
            return $ C.oscSend $ expTrig : expHost : expPort : expAddr : expTy : args

class Tuple a => OscVal a where
    getOscTypes :: a -> String
    getOscRef :: a -> SE (Ref a)

instance OscVal Sig where
    getOscTypes = const "f"
    getOscRef = newCtrlRef

instance OscVal Str where
    getOscTypes = const "s"
    getOscRef = newRef

instance (OscVal a, OscVal b) => OscVal (a, b) where
    getOscTypes (a, b) = getOscTypes a ++ getOscTypes b
    getOscRef (a, b) = do
        refA <- getOscRef a
        refB <- getOscRef b
        return $ concatRef refA refB

instance (OscVal a, OscVal b, OscVal c) => OscVal (a, b, c) where
    getOscTypes (a, b, c) = getOscTypes a ++ getOscTypes b ++ getOscTypes c
    getOscRef (a, b, c) = do
        refA <- getOscRef a
        refB <- getOscRef b
        refC <- getOscRef c
        return $ concatRef3 refA refB refC

instance (OscVal a, OscVal b, OscVal c, OscVal d) => OscVal (a, b, c, d) where
    getOscTypes (a, b, c, d) = getOscTypes a ++ getOscTypes b ++ getOscTypes c ++ getOscTypes d
    getOscRef (a, b, c, d) = do
        refA <- getOscRef a
        refB <- getOscRef b
        refC <- getOscRef c
        refD <- getOscRef d
        return $ concatRef4 refA refB refC refD

instance (OscVal a, OscVal b, OscVal c, OscVal d, OscVal e) => OscVal (a, b, c, d, e) where
    getOscTypes (a, b, c, d, e) = getOscTypes a ++ getOscTypes b ++ getOscTypes c ++ getOscTypes d ++ getOscTypes e
    getOscRef (a, b, c, d, e) = do
        refA <- getOscRef a
        refB <- getOscRef b
        refC <- getOscRef c
        refD <- getOscRef d
        refE <- getOscRef e
        return $ concatRef5 refA refB refC refD refE

-- | Listens for tuples of continuous signals read from OSC-channel.
--
-- > listenOscVal ref address initValue
listenOscVal :: (Tuple a, OscVal a) => OscRef -> String -> a -> SE a
listenOscVal port path initVal = do
    ref <- getOscRef initVal
    runEvt (listenOsc port path (getOscTypes initVal)) $ \a -> writeRef ref a
    readRef ref
