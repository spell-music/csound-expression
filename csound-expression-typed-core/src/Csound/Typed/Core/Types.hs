-- | Csound types
module Csound.Typed.Core.Types
  ( module X
  , withInits
  , withDs
  , withD
  , withSig
  , withSigs
  , withTab
  , withTabs
  ) where

import Csound.Typed.Core.Types.Gen       as X
import Csound.Typed.Core.Types.Prim      as X
import Csound.Typed.Core.Types.Tuple     as X
import Csound.Typed.Core.Types.SE        as X
import Csound.Typed.Core.Types.SE.Logic  as X
import Csound.Typed.Core.Types.SE.Ref    as X
import Csound.Typed.Core.Types.SE.Port   as X
import Csound.Typed.Core.Types.SE.Instr  as X
import Csound.Typed.Core.Types.Array     as X
import Csound.Typed.Core.Types.PureArray as X
import Csound.Typed.Core.Types.Rate      as X

import Csound.Dynamic (E)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)

-- appends inits

-- | Appends initialisation arguments. It's up to user to supply arguments with the right types. For example:
--
-- > oscil 0.5 440 sinWave `withInits` (0.5 :: D)
withInits :: (Tuple b, Tuple a) => a -> b -> a
withInits a b = genWithInits a (fromTuple b)

-- | A special case of @withInits@. Here all inits are numbers.
withDs :: Tuple a => a -> [D] -> a
withDs a ds = genWithInits a (mapM toE ds)

-- | Appends an init value which is a number.
withD :: Tuple a => a -> D -> a
withD = withInits

-- | A special case of @withInits@. Here all inits are signals.
withSigs :: Tuple a => a -> [Sig] -> a
withSigs a sigs = genWithInits a (mapM toE sigs)

-- | Appends an init value which is a signal.
withSig :: Tuple a => a -> Sig -> a
withSig = withInits

-- | A special case of @withInits@. Here all inits are arrays.
withTabs :: Tuple a => a -> [Tab] -> a
withTabs a tabs = genWithInits a (mapM toE tabs)

-- | Appends an init value which is a table.
withTab :: Tuple a => a -> Tab -> a
withTab = withInits

genWithInits :: (Tuple a) => a -> Run [E] -> a
genWithInits a vals = toTuple $ do
    as <- fromTuple a
    vs <- vals
    return $ fmap (\x -> Dynamic.withInits x vs) as
