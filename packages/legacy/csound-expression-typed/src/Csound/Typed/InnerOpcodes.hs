module Csound.Typed.InnerOpcodes(
    changed
) where

import Csound.Typed.Types.Prim
import Csound.Dynamic

-- | This opcode outputs a trigger signal that informs when any one of its k-rate 
-- arguments has changed. Useful with valuator widgets or MIDI controllers.
--
-- > ktrig changed kvar1 [, kvar2,..., kvarN]
--
-- doc: <http://www.csounds.com/manual/html/changed.html>
changed :: [Sig] -> Sig
changed = Sig . fmap f . mapM toGE
    where f = opcs "changed" [(Kr, repeat Kr)]
