{-# Language ScopedTypeVariables #-}
-- | The port is a tool to route the auio signals between instruments.
-- We can allocate the port at the instance of the instrument (at the note)
-- and pass the reference in the note to another instrument. That instrument
-- cn write a signal to the port or can read the singals.
module Csound.Typed.GlobalState.Port(
    IsPort(..), mixPort, modifyPort,
    Port(..), freePort, 
    PortCtrl(..), freePortCtrl
) where

import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.Prim

import Csound.Typed.GlobalState.Opcodes(freeChn, chnName, chnget, chnset, chngetK, chnsetK)    

-- port class

class IsPort p where 
    readPort  :: Sigs a => p a -> SE a
    writePort :: Sigs a => p a -> a -> SE ()

mixPort :: (Sigs a) => IsPort port => port a -> a -> SE ()
mixPort p value = modifyPort p (value + )

modifyPort :: (Sigs a, IsPort port) => port a -> (a -> a) -> SE ()
modifyPort p f = do
    value <- readPort p 
    writePort p $ f value

-- port for audio signals

newtype Port a = Port { unPort :: GE E }

freePort :: forall a . Sigs a => SE (Port a)
freePort = SE $ fmap (Port . return) $ freeChn

instance Sigs a => Tuple (Port a) where
    tupleMethods = makeTupleMethods to from
        where
            to :: D -> Port a
            to =  Port . toGE 

            from :: Port a -> D
            from (Port e) = fromGE e

instance Sigs a => Arg (Port a) where

instance IsPort Port where
    readPort port = SE $ hideGEinDep $ do
        names <- getNames port
        return $ fmap (toTuple . return) $ mapM chnget names

    writePort port a = SE $ do
        (names, values) <- lift getNamesAndValues
        zipWithM_ chnset names values
        where 
            getNamesAndValues = do
                names  <- getNames port
                values <- fromTuple a            
                return (names, values)

-------------------------------------------------------------
-- ports for control signals

newtype PortCtrl a = PortCtrl { unPortCtrl :: GE E }

freePortCtrl :: forall a . Sigs a => SE (PortCtrl a)
freePortCtrl = SE $ fmap (PortCtrl . return) $ freeChn

instance Sigs a => Tuple (PortCtrl a) where
    tupleMethods = makeTupleMethods to from
        where
            to :: D -> PortCtrl a
            to =  PortCtrl . toGE 

            from :: PortCtrl a -> D
            from (PortCtrl e) = fromGE e

instance Sigs a => Arg (PortCtrl a) where

instance IsPort PortCtrl where
    readPort port = SE $ hideGEinDep $ do
        names <- getNamesCtrl port
        return $ fmap (toTuple . return) $ mapM chngetK names

    writePort port a = SE $ do
        (names, values) <- lift getNamesAndValues
        zipWithM_ chnsetK names values
        where 
            getNamesAndValues = do
                names  <- getNamesCtrl port
                values <- fromTuple a            
                return (names, values)

-------------------------------------------------------

getNames :: forall a . Sigs a => Port a -> GE [E]
getNames (Port ref) = do
    idx <- ref
    return $ fmap (flip chnName idx) [1 .. (tupleArity ((error "No def here") :: a))]

getNamesCtrl :: forall a . Sigs a => PortCtrl a -> GE [E]
getNamesCtrl (PortCtrl ref) = do
    idx <- ref
    return $ fmap (flip chnName idx) [1 .. (tupleArity ((error "No def here") :: a))]
