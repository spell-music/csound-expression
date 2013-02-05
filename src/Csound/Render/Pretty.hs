module Csound.Render.Pretty where

import Data.Char(toLower)
import Text.PrettyPrint

import Csound.Exp

ppRate :: Rate -> Doc
ppRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

ppRatedVar :: RatedVar -> Doc
ppRatedVar (RatedVar r x) = ppRate r <> int x

ppOuts :: [RatedVar] -> Doc
ppOuts xs = hsep $ punctuate comma $ map ppRatedVar xs

assign :: Doc -> Doc
assign x = char '=' <+> x

ppOpc :: String -> [Doc] -> Doc
ppOpc name xs = text name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name -> ppVarType ty <> ppRate rate <> text name
    VarVerbatim _ name -> text name

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar -> empty
    GlobalVar -> char 'g'

ppPrim :: Prim -> Doc
ppPrim x = case x of
    P n -> char 'p' <> int n
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> text s
    PrimTab f -> ppTab f
    
ppTab :: Tab -> Doc
ppTab (Tab size n xs) = text "gen" <> int n <+> int size <+> (hsep $ map double xs)
 

ppIf :: Doc -> Doc -> Doc -> Doc
ppIf p t e = p <+> char '?' <+> t <+> char ':' <+> e

ppStrget :: Int -> Doc
ppStrget n = ppOpc "strget" [char 'p' <> int n]

-- file

newline = char '\n'

ppCsdFile flags instr0 instrs scores = 
    tag "CsoundSynthesizer" [
        tag "CsOptions" [flags],
        tag "CsInstruments" [instr0],
        tag "CsScore" [scores]]        

tag :: String -> [Doc] -> Doc
tag name content = vcat $ punctuate newline [
    char '<' <> text name <> char '>', 
    vcat $ punctuate newline content, 
    text "</" <> text name <> char '>']  

-- instrument

ppInstr :: Int -> Doc -> Doc
ppInstr instrId body = vcat [
    text "instr" <+> int instrId,
    body ,
    text "endin"]



