module G where

import Data.Default
import BoxModel(Scene)


data Orient = Hor | Ver

newtype Handle = Handle { unHandle :: Maybe String }

data Border 
    = NoBorder | DownBoxBorder | UpBoxBorder | EngravedBorder 
    | EmbossedBorder | BlackLine | ThinDown | ThinUp

data Diap = Diap 
    { diapMin   :: Double 
    , diapMax   :: Double }

data ScaleType = Linear | Exponential

data Font = Helvetica | Courier | Times | Symbol | Screen | Dingbats

-- defaults 

instance Default Orient where def = Hor
instance Default Handle where def = Handle def
instance Default Border where def = NoBorder
instance Default ScaleType where def = Linear
instance Default Diap where def = Diap 0 1

-- GUIs

data Window = Single Panel | Tabs String [Panel]

data Panel = Panel 
    { panelName     :: String
    , panelContent  :: Scene Border Widget }

data Widget = Widget
    { widgetName    :: String
    , widgetType    :: FlType }

data ContParam = ContParam
data RollerParam = RollerParam

data FlType 
    = Valuator
        { valuatorDiap      :: Diap
        , valuatorScaleType :: ScaleType
        , valuatorDisp      :: Handle
        , valyatorType      :: ValuatorType }
    | Joy 
        { joyX          :: ContParam
        , joyY          :: ContParam }
    | Count 
        { countStep1    :: Int
        , countStep2    :: Int
        , countType     :: CountType }
    | Box 
        { boxType       :: BoxType
        , boxFont       :: Font
        , boxFontSize   :: Int }
    | Button 
        { buttonType    :: ButtonType }
    | ButBank 
        { butBankNumX   :: Int
        , butBankNumY   :: Int
        , butBankType   :: ButtonType }

-- valuators

data ValuatorType 
    = Slider
        { sliderOrient  :: Orient
        , sliderType    :: SliderType }
    | Knob
        { knobType      :: KnobType }
    | Roller
        { rollerOrient  :: Orient
        , rollerStep    :: Double }
    | Text
        { textType      :: TextType   
        , textStep      :: Double } 

data KnobType   = ThreeD (Maybe Int) | Pie | Clock | Flat
data SliderType = Fill | Engraved | Nice | Upbox 
data CountType  = CountType -- lack of docs
data TextType   = NormalText | NoDrag | NoEdit

data BoxType    
    = FlatBox | UpBox | DownBox | ThinUpBox | ThinDownBox 
    | EngravedBox | EmbossedBox | BorderBox | ShadowBox
    | Roundedbox | RoundedShadowBox | RoundedFlatBox
    | RoundedUpBox | RoundedDownBox | DiamondUpBox 
    | DiamondDownBox | OvalBox | OvalShadowBox | OvalFlatBox

data ButtonType = ButtonType
    { isPlastic :: Bool
    , buttonTypeVal :: ButtonTypeVal }

data ButtonTypeVal = NormalButton | LightButton | CheckButton | RoundButton

instance Default SliderType     where def = Fill
instance Default KnobType       where def = Flat
instance Default RollerParam    where def = RollerParam
instance Default TextType       where def = NormalText

