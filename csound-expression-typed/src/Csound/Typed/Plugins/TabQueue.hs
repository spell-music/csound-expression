module Csound.Typed.Plugins.TabQueue(
    tabQueue2_append, tabQueue2_delete, tabQueue2_hasElements, tabQueue2_readLastElement
) where

import Data.Boolean
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(tabQueue2Plugin)

-------------------------------------------------------------------------------
-- table queue for midi notes

-- |
-- > tabQueue2_append table (pch, vol)
tabQueue2_append :: Tab -> (D, D) -> SE ()
tabQueue2_append tab (pch, vol) = SE $ (depT_ =<<) $ lift $ do    
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab <*> toGE pch <*> toGE vol
    where f tab pch vol = opcs "TabQueue2_Append" [(Xr, [Ir, Ir, Ir])] [tab, pch, vol]

-- | Delete by pitch
--
-- > tabQueue2_delete table pch
tabQueue2_delete :: Tab -> D -> SE ()
tabQueue2_delete tab pch = SE $ (depT_ =<<) $ lift $ do    
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab <*> toGE pch
    where f tab pch = opcs "TabQueue2_Delete" [(Xr, [Ir, Ir])] [tab, pch]

-- | Queue is not empty
tabQueue2_hasElements :: Tab -> BoolSig
tabQueue2_hasElements = (==* 1) . tabQueue2_hasElements'

tabQueue2_hasElements' :: Tab -> Sig
tabQueue2_hasElements' tab = fromGE $ do
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab
    where f tab = opcs "TabQueue2_HasElements" [(Kr, [Ir])] [tab]

tabQueue2_readLastElement :: Tab -> (Sig, Sig)
tabQueue2_readLastElement tab = toTuple $ fmap ($ 2) $ do
    addUdoPlugin E.tabQueue2Plugin    
    f <$> toGE tab
    where f tab = mopcs "TabQueue2_ReadLastElement" ([Kr, Kr], [Ir]) [tab]
