-- | Here you will find all opcodes from the Csound floss manual (<http://en.flossmanuals.net/csound/overview/>). 
-- If you are missing some opcodes feel free to use "Csound.LowLevel". It's easy. If it's some opcode you like a lot you can send
-- it to me by email and I will include it here.
--
-- All opcodes are defined without initialisation arguments. If you want to supply the auxiliary arguments
-- use the function 'Csound.Types.withInits' or 'Csound.Types.withDs'.
module Csound.Opcode (
    module Csound.Opcode.Basic,
    module Csound.Opcode.Advanced,
    module Csound.Opcode.Data,
    module Csound.Opcode.Interaction
) where

import Csound.Opcode.Basic
import Csound.Opcode.Advanced
import Csound.Opcode.Data
import Csound.Opcode.Interaction

       




