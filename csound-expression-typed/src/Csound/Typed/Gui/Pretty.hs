module Csound.Typed.Gui.Pretty (
  ppProc, ppMoOpc, ppVar, varPrefix, ppVarType, ppRate,

  intProp, getScale, getLabelType, getDefOrient, getOrient, getKnobType,
  getKnobCursorSize, getRollerType, getSliderType, getTextType, getBoxType,
  getFontSize, getIntFontSize, getFontType, getButtonType, getButtonBankType,
  getToggleType, appMaterial, getColor1, getColor2, getTextColor, genGetColor
) where

import Data.Char
import Data.Default
import Data.Colour.Names(white, gray, black)
import Data.Colour.SRGB

import Text.PrettyPrint.Leijen(Doc, int, hcat, hsep, punctuate, comma, text, char)
import qualified Text.PrettyPrint.Leijen as P((<+>), empty)

import Csound.Dynamic(Var(..), VarType(..), Rate(..))

import Csound.Typed.Gui.Types

-------------------------------------------------------------
-- pretty printers

ppProc :: String -> [Doc] -> Doc
ppProc name xs = text name P.<+> (hsep $ punctuate comma xs)

ppMoOpc :: [Doc] -> String -> [Doc] -> Doc
ppMoOpc outs name ins = f outs P.<+> text name P.<+> f ins
    where f = hsep . punctuate comma

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name   -> hcat [ppVarType ty, ppRate rate, text (varPrefix ty : name)]
    VarVerbatim _ name -> text name

varPrefix :: VarType -> Char
varPrefix x = case x of
    LocalVar  -> 'l'
    GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar  -> P.empty
    GlobalVar -> char 'g'

ppRate :: Rate -> Doc
ppRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show

------------------------------------------------------------------
-- Converting readable properties to integer codes

maybeDef :: Default a => Maybe a -> a
maybeDef = maybe def id

intProp :: Default a => (PropCtx -> Maybe a) -> (a -> Int) -> (PropCtx -> Doc)
intProp select convert = int . convert . maybeDef . select

getScale :: ValScaleType -> Doc
getScale x = int $ case x of
    Linear      -> 0
    Exponential -> -1

getLabelType :: PropCtx -> Doc
getLabelType = intProp ctxLabelType $ \x -> case x of
    NormalLabel     -> 0
    NoLabel         -> 1
    SymbolLabel     -> 2
    ShadowLabel     -> 3
    EngravedLabel   -> 4
    EmbossedLabel   -> 5

getDefOrient :: Rect -> Orient
getDefOrient r
    | height r < width r    = Hor
    | otherwise             = Ver

getOrient :: Orient -> PropCtx -> Orient
getOrient defOrient = maybe defOrient id . ctxOrient

getKnobType :: PropCtx -> Doc
getKnobType = intProp ctxKnobType $ \x -> case x of
    Flat        -> 4
    Pie         -> 2
    Clock       -> 3
    ThreeD _    -> 1

getKnobCursorSize :: PropCtx -> [Doc]
getKnobCursorSize ctx = case maybeDef $ ctxKnobType ctx of
    ThreeD (Just n) -> [int n]
    _               -> []

getRollerType :: Orient -> PropCtx -> Doc
getRollerType defOrient ctx = int $ case getOrient defOrient ctx of
    Hor -> 1
    Ver -> 2

getSliderType :: Orient -> PropCtx -> Doc
getSliderType defOrient ctx = int $ appMaterial ctx $
    case (getOrient defOrient ctx, maybeDef $ ctxSliderType ctx) of
        (Hor, Fill)         -> 1
        (Ver, Fill)         -> 2
        (Hor, Engraved)     -> 3
        (Ver, Engraved)     -> 4
        (Hor, Nice)         -> 5
        (Ver, Nice)         -> 6

getTextType :: PropCtx -> Doc
getTextType = intProp ctxTextType $ \x -> case x of
    NormalText  -> 1
    NoDrag      -> 2
    NoEdit      -> 3

getBoxType :: PropCtx -> Doc
getBoxType = intProp ctxBoxType $ (+1) . fromEnum

getFontSize :: PropCtx -> Doc
getFontSize  = int . getIntFontSize

getIntFontSize :: PropCtx -> Int
getIntFontSize ctx = maybe defFontSize id $ ctxFontSize ctx

getFontType :: PropCtx -> Doc
getFontType ctx = int $
    case (maybeDef $ ctxFontType ctx, maybeDef $ ctxEmphasis ctx) of
        (Helvetica, NoEmphasis)         -> 1
        (Helvetica, Bold)               -> 2
        (Helvetica, Italic)             -> 3
        (Helvetica, BoldItalic)         -> 4
        (Courier, NoEmphasis)           -> 5
        (Courier, Bold)                 -> 6
        (Courier, Italic)               -> 7
        (Courier, BoldItalic)           -> 8
        (Times, NoEmphasis)             -> 9
        (Times, Bold)                   -> 10
        (Times, Italic)                 -> 11
        (Times, BoldItalic)             -> 12
        (Symbol, _)                     -> 13
        (Screen, Bold)                  -> 15
        (Screen, _)                     -> 14
        (Dingbats, _)                   -> 16



getButtonType :: PropCtx -> Doc
getButtonType ctx = int $ appMaterial ctx 1

getButtonBankType :: PropCtx -> Doc
getButtonBankType ctx = ($ ctx) $ intProp ctxButtonType $ \x ->
    reactOnNoPlasticForRoundBug $ appMaterial ctx $ case x of
        NormalButton    -> 1
        LightButton     -> 2
        CheckButton     -> 3
        RoundButton     -> 4

getToggleType :: PropCtx -> Doc
getToggleType ctx = ($ ctx) $ intProp ctxButtonType $ \x ->
    reactOnNoPlasticForRoundBug $ appMaterial ctx $ case x of
        NormalButton    -> 2
        LightButton     -> 2
        CheckButton     -> 3
        RoundButton     -> 4

reactOnNoPlasticForRoundBug :: Int -> Int
reactOnNoPlasticForRoundBug x
  | x == 24 = 4
  | otherwise = x

appMaterial :: PropCtx -> Int -> Int
appMaterial ctx = case maybeDef $ ctxMaterial ctx of
    Plastic   -> (+ 20)
    NoPlastic -> id

getColor1 :: PropCtx -> Doc
getColor1       = genGetColor gray  ctxColor1

getColor2 :: PropCtx -> Doc
getColor2       = genGetColor white ctxColor2

getTextColor :: PropCtx -> Doc
getTextColor    = genGetColor black ctxTextColor

genGetColor :: Color -> (PropCtx -> Maybe Color) -> PropCtx -> Doc
genGetColor defColor select ctx = colorToDoc $ maybe defColor id $ select ctx
    where
      colorToDoc col = hcat $ punctuate comma
            $ fmap (channelToDoc col) [channelRed, channelGreen, channelBlue]

      channelToDoc col chn = int $ fromEnum $ chn $ toSRGB24 $ col
