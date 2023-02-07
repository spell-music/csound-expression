-- | Imperative branching constructs like if-then-else
module Csound.Typed.Core.Types.SE.Logic
  ( when1, whenD1, whens, whenDs
  , untilDo, untilDoD
  , whileDo, whileDoD
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Maybe

import Csound.Dynamic (E, DepT, CodeBlock, IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.SE

-- | With current rate tracking we assure that if we are inside
-- Kr if like block then all nested blocks are also Kr.
-- But if it's not set (it's top -level expression) then it's
-- set by user typing preference BoolD becomes Ir and BoolSig becomes Kr
withRate :: IfRate -> (IfRate -> SE a) -> SE a
withRate defRate act = SE $ do
  rate <- lift $ fromMaybe defRate <$> State.getCurrentRate
  State.withCurrentRate rate (unSE $ act rate)

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
untilDo cond act = ifBlockBy IfKr Dynamic.untilBlock cond act

whileDo :: BoolSig -> SE () -> SE ()
whileDo cond act = ifBlockBy IfKr Dynamic.whileBlock cond act

untilDoD :: BoolD -> SE () -> SE ()
untilDoD cond act = ifBlockBy IfIr Dynamic.untilBlock cond act

whileDoD :: BoolD -> SE () -> SE ()
whileDoD = ifBlockBy IfIr Dynamic.whileBlock

----------------------------------------------------------------------------------
-- utils

whenBy :: Val bool => IfRate -> [(bool, SE ())] -> SE () -> SE ()
whenBy ifRate bodies el = case bodies of
    []   -> el
    _    -> withRate ifRate $ \rate -> SE $ join $ lift $ do
        checksE <- mapM (toE . fst) bodies
        let bodiesE = fmap (Dynamic.toBlock . unSE . snd) bodies
            elE = Dynamic.toBlock $ unSE el
        pure $ Dynamic.whens rate (zip checksE bodiesE) elE

-- | Invokes the given procedure if the boolean signal is true.
when1By :: (IsPrim bool, Val bool, PrimOf bool ~ Bool)
  => IfRate -> bool -> SE () -> SE ()
when1By ifRate xp body = case getPrim xp of
    Just p -> if p then body else return ()
    _      -> ifBlockBy ifRate Dynamic.when1 xp body

-- | Constructs generic if-block statement with single then case
-- We can choose constructors for: if, while, until statements
ifBlockBy :: Val cond => IfRate -> (IfRate -> E -> DepT Run (CodeBlock E) -> DepT Run ()) -> cond -> SE () -> SE ()
ifBlockBy defRate cons p body = withRate defRate $ \rate ->
  SE $ do
    pE <- lift $ toE p
    cons rate pE (Dynamic.toBlock $ unSE body)
