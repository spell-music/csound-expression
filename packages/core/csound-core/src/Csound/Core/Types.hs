-- | Csound types
module Csound.Core.Types
  ( module X
  , withInits
  , withDs
  , withD
  , withSig
  , withSigs
  , withTab
  , withTabs
  , mean
  ) where

import Csound.Core.Types.Gen       as X
import Csound.Core.Types.Prim      as X
import Csound.Core.Types.Tuple     as X
import Csound.Core.Types.SE        as X
import Csound.Core.Types.Array     as X
import Csound.Core.Types.Rate      as X
import Csound.Core.Types.SigSpace  as X

import Csound.Dynamic (E)
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Run)

-- | Mean value.
mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

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
