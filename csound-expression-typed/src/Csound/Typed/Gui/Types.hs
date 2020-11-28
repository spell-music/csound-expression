{-# Language CPP #-}
module Csound.Typed.Gui.Types (
    Props(..),
    Prop(..), BorderType(..), Color,
    Rect(..), FontType(..), Emphasis(..),
    Material(..), Orient(..), LabelType(..),
    ScaleFactor,

    ValDiap(..), ValStep, ValScaleType(..), ValSpan(..),
    linSpan, expSpan, uspan, bspan, uspanExp,
    KnobType(..),
    SliderType(..),
    TextType(..),
    BoxType(..),
    ButtonType(..),

    defFontSize,

    PropCtx(..), setPropCtx, getLabel
) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Control.Applicative(Alternative(..))
import Data.Default
import Data.Colour

import Csound.Typed.Gui.BoxModel(Rect(..))

-- | The Csound colours.
type Color = Colour Double

-- | The orientation of the widget (slider, roller). This property is
-- never needs to be set in practice. If this property is not set then
-- default orientation is calculated from the bounding box of the widget.
-- If the width is greater than the height then we need to use a horizontal
-- widget otherwise it should be a vertical one.
data Orient = Hor | Ver

-- | A value span is a diapason of the value and a type
-- of the scale (can be linear or exponential).
data ValSpan = ValSpan
    { valSpanDiap  :: ValDiap
    , valSpanScale :: ValScaleType }

-- | Makes a linear @ValSpan@ with specified boundaries.
--
-- > linSpan minVal maxVal
linSpan :: Double -> Double -> ValSpan
linSpan a b = ValSpan (ValDiap a b) Linear

-- | Makes an exponential @ValSpan@ with specified boundaries.
--
-- > expSpan minVal maxVal
expSpan :: Double -> Double -> ValSpan
expSpan a b = ValSpan (ValDiap (checkBound a) b) Exponential
    where
        checkBound x
            | x <= 0    = 0.00001
            | otherwise = x

-- | Unit span. A special case:
--
-- > uspan = linSpan 0 1
uspan :: ValSpan
uspan = linSpan 0 1

-- | Bipolar unit span. A special case:
--
-- > uspan = linSpan (-1) 1
bspan :: ValSpan
bspan = linSpan (-1) 1

-- | An exponential unit span. A special case:
--
-- > uspan = expSpan 0 1
uspanExp :: ValSpan
uspanExp = linSpan 0 1

-- | The diapason of the continuous value.
data ValDiap = ValDiap
    { valDiapMin   :: Double
    , valDiapMax   :: Double }

data ValScaleType = Linear | Exponential

type ValStep = Double

data FontType       = Helvetica | Courier | Times | Symbol | Screen | Dingbats
data Emphasis       = NoEmphasis | Italic | Bold | BoldItalic
data KnobType       = ThreeD (Maybe Int) | Pie | Clock | Flat
data SliderType     = Fill | Engraved | Nice
data TextType       = NormalText | NoDrag | NoEdit

-- | The type of the material of the element. It affects sliders and buttons.
data Material       = NoPlastic | Plastic

-- | Some values are not implemented on the Csound level.
data LabelType      = NormalLabel | NoLabel | SymbolLabel
                    | ShadowLabel | EngravedLabel | EmbossedLabel

-- | The type of the box. Some values are not implemented on the Csound level.
data BoxType
    = FlatBox
    | UpBox
    | DownBox
    | ThinUpBox
    | ThinDownBox
    | EngravedBox
    | EmbossedBox
    | BorderBox
    | ShadowBox
    | Roundedbox
    | RoundedShadowBox
    | RoundedFlatBox
    | RoundedUpBox
    | RoundedDownBox
    | DiamondUpBox
    | DiamondDownBox
    | OvalBox
    | OvalShadowBox
    | OvalFlatBox
    deriving (Enum)

data BorderType
    = NoBorder
    | DownBoxBorder
    | UpBoxBorder
    | EngravedBorder
    | EmbossedBorder
    | BlackLine
    | ThinDown
    | ThinUp
    deriving (Enum)

-- | The type of the button. It affects toggle buttons and button banks.
--
-- In Csound buttons and toggle buttons
-- are constructed with the same function (but with different button types).
-- But in this library they are contructed by different functions (@button@ and @toggle@).
-- Normal button is a plain old button, but other values specify toggle buttons.
-- So this property doesn't affect the buttons (since they could be only normal buttons).
data ButtonType = NormalButton | LightButton | CheckButton | RoundButton

defFontSize :: Int
defFontSize = 15

instance Default FontType       where def = Courier
instance Default Emphasis       where def = NoEmphasis
instance Default SliderType     where def = Fill
instance Default KnobType       where def = Flat
instance Default TextType       where def = NormalText
instance Default ButtonType     where def = NormalButton
instance Default BoxType        where def = FlatBox
instance Default Material       where def = Plastic
instance Default LabelType      where def = NormalLabel

data Props = Props
    { propsBorder   :: Maybe BorderType
    , propsScaleFactor :: Maybe ScaleFactor
    , otherProps    :: [Prop] }

type ScaleFactor = (Double, Double)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Props where
    (<>) = mappendProps

instance Monoid Props where
    mempty  = def

#else

instance Monoid Props where
    mempty  = def
    mappend = mappendProps

#endif


mappendProps :: Props -> Props -> Props
mappendProps a b = Props { propsBorder = propsBorder a <|> propsBorder b
                    , propsScaleFactor = propsScaleFactor a <|> propsScaleFactor b
                    , otherProps  = mappend (otherProps a) (otherProps b) }

instance Default Props where
    def = Props Nothing Nothing []

-- | Properties of the widgets.
data Prop
    = SetLabel String
    | SetMaterial Material
    | SetBoxType BoxType
    | SetColor1 Color | SetColor2 Color | SetTextColor Color
    | SetFontSize Int | SetFontType FontType | SetEmphasis Emphasis
    | SetSliderType SliderType
    | SetTextType TextType
    | SetButtonType ButtonType
    | SetOrient Orient
    | SetKnobType KnobType
    | SetLabelType LabelType

-----------------------------------------------------------
-- cascading context, here we group properties by type

data PropCtx = PropCtx
    { ctxLabel        :: Maybe String
    , ctxMaterial     :: Maybe Material
    , ctxLabelType    :: Maybe LabelType
    , ctxBoxType      :: Maybe BoxType
    , ctxColor1       :: Maybe Color
    , ctxColor2       :: Maybe Color
    , ctxTextColor    :: Maybe Color
    , ctxFontSize     :: Maybe Int
    , ctxFontType     :: Maybe FontType
    , ctxEmphasis     :: Maybe Emphasis
    , ctxOrient       :: Maybe Orient
    , ctxSliderType   :: Maybe SliderType
    , ctxButtonType   :: Maybe ButtonType
    , ctxTextType     :: Maybe TextType
    , ctxKnobType     :: Maybe KnobType }

instance Default PropCtx where
    def = PropCtx Nothing Nothing Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing Nothing Nothing Nothing
                  Nothing Nothing Nothing

setPropCtx :: Prop -> PropCtx -> PropCtx
setPropCtx p x = case p of
            SetLabel        a -> x { ctxLabel  = Just a }
            SetMaterial     a -> x { ctxMaterial = Just a }
            SetLabelType    a -> x { ctxLabelType = Just a }
            SetBoxType      a -> x { ctxBoxType = Just a }
            SetColor1       a -> x { ctxColor1 = Just a }
            SetColor2       a -> x { ctxColor2 = Just a }
            SetTextColor    a -> x { ctxTextColor = Just a }
            SetFontSize     a -> x { ctxFontSize = Just a }
            SetFontType     a -> x { ctxFontType = Just a }
            SetEmphasis     a -> x { ctxEmphasis = Just a }
            SetOrient       a -> x { ctxOrient = Just a }
            SetSliderType   a -> x { ctxSliderType = Just a }
            SetButtonType   a -> x { ctxButtonType = Just a }
            SetTextType     a -> x { ctxTextType = Just a }
            SetKnobType     a -> x { ctxKnobType = Just a }

getLabel :: PropCtx -> String
getLabel = maybe "" id . ctxLabel
