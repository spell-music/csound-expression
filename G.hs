module G where

import Data.Default

data Orient = Hor | Ver

newtype Handle = Handle { unHandle :: Maybe String }

type Measure = Maybe Int

data Size = Size 
    { height :: Measure
    , width  :: Measure }

data Offset = Offset 
    { offsetX :: Measure
    , offsetY :: Measure }

data Place = Place
    { placeSize     :: Size 
    , placeOffset   :: Offset }

data Border 
    = NoBorder | DownBox | UpBox | EngravedBorder 
    | EmbossedBorder | BlackLine | ThinDown | ThinUp

data Diap = Diap 
    { diapMin   :: Double 
    , diapMax   :: Double }

data ScaleType = Linear | Exponential

data Font = Helvetica | Courier | Times | Symbol | Screen | Dingbats

-- defaults 

instance Default Size   where def = Size def def
instance Default Offset where def = Offset def def
instance Default Place  where def = Place def def
instance Default Orient where def = Hor
instance Default Handle where def = Handle def
instance Default Border where def = NoBorder
instance Default ScaleType where def = Linear
instance Default Diap where def = Diap 0 1

-- GUIs

data Fl = Fl 
    { flPlace   :: Place
    , flLabel   :: String
    , flType    :: FlType }

data FlType 
    = Container 
        { containerChildren :: [Fl]
        , containerBorder   :: Border
        , containerParam    :: ContainerType }
    | Valuator
        { valuatorDiap      :: Diap
        , valuatorScaleType :: ScaleType
        , valuatorDisp      :: Handle
        , valyatorType      :: ValuatorType }
    | Joy 
        { joyX  :: ContParam
        , joyY  :: ContParam }
    | Count 
        { countStep1 :: Int
        , countStep2 :: Int
        , countType  :: CountType }
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

-- containers        

data ContainerType 
    = Group 
    | Pack 
        { packOrient :: Orient
        , packSpace  :: Double }
    | Panel
        { panelKbdcapture :: Bool
        , panelClose :: Bool } 
    | Scroll
    | Tabs        

-- valuators

data ValuatorType 
    = Slider
        { sliderOrient      :: Orient
        , sliderType        :: SliderType
        }
    | Knob
        { knobType :: KnobType }
    | Roller
        { rollerOrient  :: Orient
        , rollerStep    :: Double }
    | Text
        { textType  :: TextType   
        , textStep  :: Double } 

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

instance Default SliderType  where def = Fill
instance Default KnobType  where def = Flat
instance Default RollerParam where def = RollerParam def 1
instance Default TextType where def = NormalText

