module Csound.Typed.Opcode.Deprecated (
  tableiw,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed

--

--
-- >  tableiw  isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- csound doc: <https://csound.com/docs/manual/tableiw.html>
tableiw :: D -> D -> Tab -> SE ()
tableiw b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "tableiw" [(Xr, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]
