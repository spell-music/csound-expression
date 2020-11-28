module Csound.Typed.Types(
    -- * Primitives
    module Csound.Typed.Types.Prim,
    module Csound.Typed.Types.Lift,
    -- * Init values
    -- | In Csound we can supply an opcodes with initialization arguments.
    -- They are optional. To imitate this functionality in haskell we
    -- can use these functions.
    withInits, withDs, withSigs, withTabs, withD, withSig, withTab, withSeed,
    -- * Tuples
    module Csound.Typed.Types.Tuple,        
    -- * Events
    module Csound.Typed.Types.Evt,

    -- * Arrays
    module Csound.Typed.Types.Array, 

    -- * Arguments for monophonic synths
    module Csound.Typed.Types.MonoArg,  

    -- * Signal space (generic signal transformers)
    module Csound.Typed.Types.SigSpace,

    -- * Tab helpers
    getNextGlobalGenId
) where

import qualified Csound.Dynamic as D

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.Evt
import Csound.Typed.Types.Lift
import Csound.Typed.Types.Array
import Csound.Typed.Types.MonoArg
import Csound.Typed.Types.SigSpace

import Csound.Typed.GlobalState(evalSE, SE, geToSe)

import qualified Csound.Typed.GlobalState as G(getNextGlobalGenId)

getNextGlobalGenId :: SE Int
getNextGlobalGenId = geToSe G.getNextGlobalGenId

-- appends inits

-- | Appends initialisation arguments. It's up to user to supply arguments with the right types. For example:
--
-- > oscil 0.5 440 sinWave `withInits` (0.5 :: D)
withInits :: (Tuple a, Tuple b) => a -> b -> a
withInits a b = genWithInits a (fromTuple b)

-- | A special case of @withInits@. Here all inits are numbers. 
withDs :: Tuple a => a -> [D] -> a
withDs a ds = genWithInits a (mapM toGE ds)

-- | Appends an init value which is a number.
withD :: Tuple a => a -> D -> a
withD = withInits

-- | A special case of @withInits@. Here all inits are signals. 
withSigs :: Tuple a => a -> [Sig] -> a
withSigs a sigs = genWithInits a (mapM toGE sigs)

-- | Appends an init value which is a signal.
withSig :: Tuple a => a -> Sig -> a
withSig = withInits

-- | A special case of @withInits@. Here all inits are arrays. 
withTabs :: Tuple a => a -> [Tab] -> a
withTabs a tabs = genWithInits a (mapM toGE tabs)

-- | Appends an init value which is a table.
withTab :: Tuple a => a -> Tab -> a
withTab = withInits

-- | Applies a seed to the random value. 
-- It's equivalent to the @withD@ but it has a special 
-- meaning of canceling the side effect. When random
-- opcode is provided with seed value it's no longer
-- contains a side effect so we don't need to restrict it.
withSeed :: SE Sig -> D -> Sig
withSeed a d = hideGE $ evalSE $ fmap (flip withD d) a

genWithInits :: (Tuple a) => a -> GE [E] -> a
genWithInits a vals = toTuple $ do
    as <- fromTuple a
    vs <- vals
    return $ fmap (\x -> D.withInits x vs) as

