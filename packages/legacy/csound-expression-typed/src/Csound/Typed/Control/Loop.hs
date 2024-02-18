module Csound.Typed.Control.Loop(
    whileDo, untilDo, forD, forSig, whileEvt,
    -- * Maybe for Csound
    Maybe'(..), nothing', just', maybe', defMaybe', onMaybe'
) where

import Data.Boolean

import qualified Csound.Dynamic as D(untilBegin, untilEnd, readVar, Var)

import Csound.Typed.Types
import Csound.Typed.Control.Ref
import Csound.Typed.GlobalState.SE

-- | Executes an action while condition in the reference is not true.
untilDo :: BoolSigOrD a => Ref a -> SE () -> SE ()
untilDo p body = do
    untilBegin (getVar p)
    body
    untilEnd

-- | Executes an action while condition in the reference is true.
whileDo :: BoolSigOrD a => Ref a -> SE () -> SE ()
whileDo p body = do
    whileBegin (getVar p)
    body
    untilEnd

getVar :: Ref a -> D.Var
getVar (Ref [a]) = a 
getVar _ = error "Not a single Var: module Csound.Typed.Control.Loop, line 27"

untilBegin :: D.Var -> SE ()
untilBegin var = fromDep_ $ do
    val <- D.readVar var    
    D.untilBegin (val)

whileBegin :: D.Var -> SE ()
whileBegin var = fromDep_ $ do
    val <- D.readVar var    
    D.untilBegin (notB val)

untilEnd :: SE ()
untilEnd = fromDep_ D.untilEnd

-- | For loop. It expects an initial counter, update counter function, predicate for counter and loop body.
-- It works at init rate.
forD :: D -> (D -> SE BoolD) -> (D -> SE D) -> (D -> SE ()) -> SE ()
forD initVal pred update body = do    
    countRef <- newRef initVal
    initCond <- pred initVal 
    ref <- newRef initCond 
    whileDo ref $ do
        count <- readRef countRef
        body count
        newCount <- update count
        writeRef ref =<< pred newCount
        writeRef countRef newCount

-- | For loop. It expects an initial counter, update counter function, predicate for counter and loop body.
-- It works at control rate.
forSig :: Sig -> (Sig -> SE BoolSig) -> (Sig -> SE Sig) -> (Sig -> SE ()) -> SE ()
forSig initVal pred update body = do    
    countRef <- newCtrlRef initVal
    initCond <- pred initVal 
    ref <- newRef initCond 
    whileDo ref $ do
        count <- readRef countRef
        body count
        newCount <- update count
        writeRef ref =<< pred newCount
        writeRef countRef newCount

data Maybe' a = Maybe' BoolSig a

nothing' :: Tuple a => Maybe' a
nothing' = Maybe' false defTuple

just' :: a -> Maybe' a
just' a = Maybe' true a

maybe' :: (Tuple b) => b -> (a -> b)-> Maybe' a -> b
maybe' defVal f (Maybe' cond val) = ifTuple cond (f val) defVal

defMaybe' :: (Tuple a) => a -> Maybe' a -> a
defMaybe' defVal (Maybe' cond val) = ifTuple cond val defVal 

onMaybe' :: (a -> SE ()) -> Maybe' a -> SE ()
onMaybe' proc (Maybe' cond val) = when1 cond (proc val)

-- | Creates an event out of while-loop. 
-- The body of the loop should produce a value.
whileEvt :: BoolSigOrD a => Ref a -> SE (Maybe' b) -> Evt b
whileEvt ref gen = Evt $ \bam -> do
    whileDo ref $ do
        mval <- gen
        onMaybe' bam mval
