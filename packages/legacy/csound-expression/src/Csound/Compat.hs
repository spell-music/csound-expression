{- | The compatibility functions. Functions to work with older versions of Csound.

The module contains opcodes that work in old versions of Csound.
Some opcodes are initialy implemented as UDOs. And if they are super cool
Csound developers decide to reimplement them in C and built in the Csound core.
It makes the opcodes faster.

But if your version of Csound is not so modern and opcode version is not
available for you. You can still use the UDO version, which is less efficient
but should work.

This module is not imported by default and intended to be used qualified.
-}
module Csound.Compat (
  module X,
) where

import Csound.Compat.Filter as X
