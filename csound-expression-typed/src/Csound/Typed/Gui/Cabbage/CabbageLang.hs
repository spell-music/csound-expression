module Csound.Typed.Gui.Cabbage.CabbageLang(
	Lang, Line(..), Property(..), Arg(..), ppCabbage
) where

import Text.PrettyPrint.Leijen

type Lang = [Line]

data Line = Line 
	{ lineDef :: String
	, lineProperties :: [Property]
	}

data Property = Property 
	{ propertyName :: String
	, propertyArgs :: [Arg] 
	}

data Arg = StringArg String | FloatArg Float | IntArg Int | ColonArg Float Float

--------------------------------------------------
-- pretty print

ppCabbage :: Lang -> Doc
ppCabbage xs = vcat $ fmap ppLine xs

ppLine :: Line -> Doc
ppLine (Line name props) = text name <+> hcat (punctuate comma (fmap ppProp props))

ppProp :: Property -> Doc
ppProp (Property name args) = text name <> tupled (fmap ppArg args)

ppArg :: Arg -> Doc
ppArg x = case x of
	StringArg s -> dquotes (text s)
	FloatArg a  -> float a
	IntArg a    -> int a
	ColonArg a b -> float a <> colon <> float b