module Csound.Render.PrettyOp where

import Text.PrettyPrint

binaries, unaries, funcs :: String -> [Doc] -> Doc

binaries op as = binary op (as !! 0) (as !! 1)
unaries  op as = unary  op (as !! 0)
funcs    op as = func   op (as !! 0)

binary :: String -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> text op <+> b

unary :: String -> Doc -> Doc
unary op a = parens $ text op <> a

func :: String -> Doc -> Doc
func op a = text op <> parens a
