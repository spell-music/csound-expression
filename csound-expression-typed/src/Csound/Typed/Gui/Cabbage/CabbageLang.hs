module Csound.Typed.Gui.Cabbage.CabbageLang(
  Lang, Line(..), Property(..), Arg(..), ppCabbage
) where

import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text

type Lang = [Line]

data Line = Line
  { lineDef :: Text
  , lineProperties :: [Property]
  }

data Property = Property
  { propertyName :: Text
  , propertyArgs :: [Arg]
  }

data Arg = StringArg Text | FloatArg Float | IntArg Int | ColonArg Float Float

--------------------------------------------------
-- pretty print

ppCabbage :: Lang -> Doc
ppCabbage xs = vcat $ fmap ppLine xs

ppLine :: Line -> Doc
ppLine (Line name props) = textStrict name <+> hcat (punctuate comma (fmap ppProp props))

ppProp :: Property -> Doc
ppProp (Property name args) = textStrict name <> tupled (fmap ppArg args)

ppArg :: Arg -> Doc
ppArg x = case x of
  StringArg s -> dquotes (textStrict s)
  FloatArg a  -> float a
  IntArg a    -> int a
  ColonArg a b -> float a <> colon <> float b
