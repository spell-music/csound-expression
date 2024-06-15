-- | Imperative branching constructs like if-then-else
module Csound.Core.Types.SE.Logic
  ( when1, whens, whileDo, untilDo, doRepeat, forEach
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Boolean
import Data.Maybe

import Csound.Dynamic (E, IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Core.State (Dep)
import Csound.Core.State qualified as State
import Csound.Core.Types.Tuple
import Csound.Core.Types.Rate
import Csound.Core.Types.Prim
import Csound.Core.Types.SE.Core
import Csound.Core.Types.SE.Ref

-- | With current rate tracking we assure that if we are inside
-- Kr if like block then all nested blocks are also Kr.
-- But if it's not set (it's top -level expression) then it's
-- set by user typing preference BoolD becomes Ir and BoolSig becomes Kr
withRate :: IfRate -> (IfRate -> SE a) -> SE a
withRate defRate act = SE $ do
  rate <- lift $ fromMaybe defRate <$> State.getCurrentRate
  State.withCurrentRate rate (unSE $ act rate)

-- | Invokes the given procedure if the boolean signal is true.
when1 :: forall bool . BoolVal bool => bool -> SE () -> SE ()
when1 xp body = when1By (boolValRate @bool) xp body

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whens :: forall bool . BoolVal bool => [(bool, SE ())] -> SE () -> SE ()
whens bodies el = whenBy (boolValRate @bool) bodies el

-- | Repeats block of statements until value in the reference wil become false
whileDo :: forall a bool . (Tuple a, BoolVal bool) => Ref a -> (a -> bool) -> SE () -> SE ()
whileDo = \case
  Ref vs -> \check body -> withRate (boolValRate @bool) $ \rate ->
    ifBlockBy rate Dynamic.whileBlock (check $ toTuple $ pure $ fmap (Dynamic.inlineVar rate) vs) body

-- | Repeats block of statements until value in the reference wil become true
untilDo :: forall a bool . (Tuple a, BoolVal bool) => Ref a -> (a -> bool) -> SE () -> SE ()
untilDo = \case
  Ref vs -> \check body -> withRate (boolValRate @bool) $ \rate ->
    ifBlockBy rate Dynamic.untilBlock (check $ toTuple $ pure $ fmap (Dynamic.inlineVar rate) vs) body

-- | Repeats statement N times, passes counter as an argument
doRepeat :: forall a . SigOrD a => a -> (a -> SE ()) -> SE ()
doRepeat maxCount body = forEach 0 (pure . (+1)) (`less` maxCount) body

forEach :: forall a . SigOrD a => a -> (a -> SE a) -> (a -> BooleanOf a) -> (a -> SE ()) -> SE ()
forEach initVal next check body = withRate (boolValRate @(BooleanOf a)) $ \case
  IfIr -> do
    ref <- newLocalRef initVal
    writeRef ref initVal
    whileDo ref check $ do
      body =<< readRef ref
      writeRef ref =<< next =<< readRef ref
  IfKr -> do
    ref <- newLocalRef (K initVal)
    writeRef ref (K initVal)
    whileDo ref (check . unK) $ do
      body . unK =<< readRef ref
      writeRef ref . K =<< next . unK =<< readRef ref

----------------------------------------------------------------------------------
-- utils

whenBy :: Val bool => IfRate -> [(bool, SE ())] -> SE () -> SE ()
whenBy ifRate bodies el = case bodies of
    []   -> el
    _    -> withRate ifRate $ \rate -> SE $ join $ lift $ do
        checksE <- mapM (toE . fst) bodies
        let bodiesE = fmap (unSE . snd) bodies
        pure $ Dynamic.whens rate (zip checksE bodiesE) (unSE el)

-- | Invokes the given procedure if the boolean signal is true.
when1By :: (IsPrim bool, Val bool, PrimOf bool ~ Bool)
  => IfRate -> bool -> SE () -> SE ()
when1By ifRate xp body = case getPrim xp of
    Just p -> if p then body else return ()
    _      -> ifBlockBy ifRate Dynamic.when1 xp body

-- | Constructs generic if-block statement with single then case
-- We can choose constructors for: if, while, until statements
ifBlockBy :: Val cond => IfRate -> (IfRate -> E -> Dep () -> Dep ()) -> cond -> SE () -> SE ()
ifBlockBy defRate cons p body = withRate defRate $ \rate ->
  SE $ do
    pE <- lift $ toE p
    cons rate pE (unSE body)
