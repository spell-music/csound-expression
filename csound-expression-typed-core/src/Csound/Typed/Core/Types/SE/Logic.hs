-- | Imperative branching constructs like if-then-else
module Csound.Typed.Core.Types.SE.Logic
  ( when1, whenD1, whens, whenDs
  , untilDo, untilDoD
  , whileDo, whileDoD
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (E, DepT, CodeBlock, IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.SE

-- | Invokes the given procedure if the boolean signal is true.
when1 :: BoolSig -> SE () -> SE ()
when1 xp body = when1By IfKr xp body

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whens :: [(BoolSig, SE ())] -> SE () -> SE ()
whens bodies el = whenBy IfKr bodies el

-- | Invokes the given procedure if the boolean signal is true.
whenD1 :: BoolD -> SE () -> SE ()
whenD1 xp body = when1By IfIr xp body

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whenDs :: [(BoolD, SE ())] -> SE () -> SE ()
whenDs bodies el = whenBy IfIr bodies el

untilDo :: BoolSig -> SE () -> SE ()
untilDo = ifBlockBy (Dynamic.untilBlock IfKr)

whileDo :: BoolSig -> SE () -> SE ()
whileDo = ifBlockBy (Dynamic.whileBlock IfKr)

untilDoD :: BoolD -> SE () -> SE ()
untilDoD = ifBlockBy (Dynamic.untilBlock IfIr)

whileDoD :: BoolD -> SE () -> SE ()
whileDoD = ifBlockBy (Dynamic.whileBlock IfIr)

----------------------------------------------------------------------------------
-- utils

whenBy :: Val bool => IfRate -> [(bool, SE ())] -> SE () -> SE ()
whenBy ifRate bodies el = case bodies of
    []   -> el
    _    -> SE $ join $ lift $ do
        checksE <- mapM (toE . fst) bodies
        let bodiesE = fmap (Dynamic.toBlock . unSE . snd) bodies
            elE = Dynamic.toBlock $ unSE el
        pure $ Dynamic.whens ifRate (zip checksE bodiesE) elE

-- | Invokes the given procedure if the boolean signal is true.
when1By :: (IsPrim bool, Val bool, PrimOf bool ~ Bool)
  => IfRate -> bool -> SE () -> SE ()
when1By ifRate xp body = case getPrim xp of
    Just p -> if p then body else return ()
    _      -> ifBlockBy (Dynamic.when1 ifRate) xp body

-- | Constructs generic if-block statement with single then case
-- We can choose constructors for: if, while, until statements
ifBlockBy :: Val cond => (E -> DepT Run (CodeBlock E) -> DepT Run ()) -> cond -> SE () -> SE ()
ifBlockBy cons p body =
  SE $ do
    pE <- lift $ toE p
    cons pE (Dynamic.toBlock $ unSE body)
