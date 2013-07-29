module Csound.Exp.Gui where

import Prelude hiding(elem, span)

import Data.Default
import Data.Monoid

import qualified Data.IntMap as IM
import qualified Csound.Render.Pretty as P
import Csound.Render.Pretty(Doc, int, double)

import Csound.Exp hiding (P)
import qualified Csound.BoxModel as Box
import Csound.BoxModel(Rect(..))

newtype GuiHandle = GuiHandle { unGuiHandle :: Int }

data Orient = Hor | Ver

data Span = Span 
    { spanDiap  :: Diap
    , spanScale :: ScaleType }

data Diap = Diap 
    { diapMin   :: Double 
    , diapMax   :: Double }

data ScaleType = Linear | Exponential

type Step = Double

data Color = Color Int Int Int

data FontType   = Helvetica | Courier | Times | Symbol | Screen | Dingbats
data Emphasis   = NoEmphasis | Italic | Bold | BoldItalic
data KnobType   = ThreeD (Maybe Int) | Pie | Clock | Flat
data SliderType = Fill | Engraved | Nice | Upbox 
data TextType   = NormalText | NoDrag | NoEdit

data BoxType    
    = FlatBox | UpBox | DownBox | ThinUpBox | ThinDownBox 
    | EngravedBox | EmbossedBox | BorderBox | ShadowBox
    | Roundedbox | RoundedShadowBox | RoundedFlatBox
    | RoundedUpBox | RoundedDownBox | DiamondUpBox 
    | DiamondDownBox | OvalBox | OvalShadowBox | OvalFlatBox
    deriving (Enum)

data BorderType
    = NoBorder | DownBoxBorder | UpBoxBorder | EngravedBorder 
    | EmbossedBorder | BlackLine | ThinDown | ThinUp

data ButtonType = ButtonType
    { isPlastic     :: Bool
    , buttonTypeVal :: ButtonTypeVal }

data ButtonTypeVal = NormalButton | LightButton | CheckButton | RoundButton

defFontSize :: Int
defFontSize = 15

instance Default FontType       where def = Courier
instance Default Emphasis       where def = NoEmphasis
instance Default SliderType     where def = Fill
instance Default KnobType       where def = Flat
instance Default TextType       where def = NormalText
instance Default ButtonType     where def = ButtonType False def
instance Default ButtonTypeVal  where def = NormalButton
instance Default BoxType        where def = FlatBox

data Elem 
    = GuiVar GuiHandle 
    
    -- valuators
    | Count  Diap Step (Maybe Step) Double
    | Joy    Span Span (Double, Double)
    | Knob   Span Double
    | Roller Span Step Double
    | Slider Span Double
    | Text   Diap Step Double

    -- other widgets  
    | Box String
    | ButBank Int Int
    | Button 
    | Value Double
    | Vkeybd

data Prop
    = SetBorder BorderType
    | SetLabel String
    | SetBoxType BoxType
    | SetColor1 Color | SetColor2 Color | SetTextColor Color
    | SetFontSize Int | SetFontType FontType | SetEmphasis Emphasis
    | SetSliderType SliderType    
    | SetTextType TextType
    | SetButtonType ButtonType 
    | SetOrient Orient
    | SetKnobType KnobType

newtype Gui = Gui { unGui :: LowGui }

type LowGui = Box.Scene [Prop] ElemWithOuts

data Win = Win 
    { winTitle :: String 
    , winRect  :: Rect
    , winGui   :: Gui }

data GuiNode = GuiNode
    { guiNodeElem   :: Gui
    , guiNodeHandle :: GuiHandle }

data ElemWithOuts = ElemWithOuts 
    { elemOuts      :: [Var]
    , elemContent   :: Elem }

type ElemOuts = [Var]

fromElem :: ElemOuts -> Elem -> Gui
fromElem as el = Gui $ Box.prim (ElemWithOuts as el)

fromGuiHandle :: GuiHandle -> Gui
fromGuiHandle = Gui . Box.prim . ElemWithOuts [] . GuiVar 

onLowGuis :: ([LowGui] -> LowGui) -> ([Gui] -> Gui)
onLowGuis f = Gui . f . fmap unGui

onLowGui1 :: (LowGui -> LowGui) -> (Gui -> Gui)
onLowGui1 f = Gui . f . unGui 

hor :: [Gui] -> Gui
hor = onLowGuis Box.hor 

ver :: [Gui] -> Gui
ver = onLowGuis Box.ver

space :: Gui
space = Gui Box.space

sca :: Double -> Gui -> Gui
sca d = onLowGui1 (Box.sca d)

padding :: Int -> Gui -> Gui
padding n = onLowGui1 (Box.padding n)

margin :: Int -> Gui -> Gui
margin n = onLowGui1 (Box.margin n)

props :: [Prop] -> Gui -> Gui
props ps = onLowGui1 (Box.appendContext ps)

type GuiMap = IM.IntMap Gui

guiMap :: [GuiNode] -> GuiMap
guiMap = IM.fromList . fmap (\(GuiNode elem (GuiHandle n)) -> (n, elem))

restoreTree :: GuiMap -> Gui -> Gui
restoreTree m x = Gui $ (unGui x) >>= rec
    where rec elem = case elemContent elem of
            GuiVar h -> unGui $ m IM.! unGuiHandle h
            _        -> return elem


drawGui :: Win -> P.Doc
drawGui w = onPanel (winTitle w) (winRect w) $ 
    renderAbsScene $ Box.draw (winRect w) $ unGui $ winGui w
    where
        renderAbsScene = Box.cascade drawPrim P.empty P.vcat setProps def
            where setProps ps = appEndo $ mconcat $ fmap (Endo . setPropCtx) ps

        onPanel title rect body = P.vcat [
            P.ppProc "FLpanel" [P.text $ show title, P.int $ width rect, P.int $ height rect],
            body,
            P.ppProc "FLpanelEnd" []]
    
drawPrim :: PropCtx -> Rect -> ElemWithOuts -> P.Doc
drawPrim ctx rect (ElemWithOuts outs elem) = case elem of
    -- valuators
    Count  diap step1 step2 initVal -> drawCount diap step1 step2 initVal
    Joy    span1 span2 initVal      -> drawJoy span1 span2 initVal
    Knob   span initVal             -> drawKnob span initVal
    Roller span step initVal        -> drawRoller span step initVal
    Slider span initVal             -> drawSlider span initVal
    Text   diap step initVal        -> drawText diap step initVal

    -- other widgets  
    Box text                        -> drawBox text 
    ButBank xn yn                   -> drawButBank xn yn 
    Button                          -> drawButton 
    Value initVal                   -> drawValue initVal
    Vkeybd                          -> drawVkeybd 

    -- error
    GuiVar guiHandle        -> error $ "orphan handle: " ++ (show $ unGuiHandle $ guiHandle)
    where
        f = fWithLabel (getLabel ctx)
        fWithLabel label name args = P.ppMoOpc (fmap P.ppVar outs) name ((P.text $ show $ label) : args)
        frame = frameBy rect
        frameBy x = fmap int [width x, height x, px x, py x]       
        noDisp = int (-1)
        noOpc  = int (-1)
        drawSpan (Span diap scale) = [imin diap, imax diap, getScale scale]
   
        imin = double . diapMin
        imax = double . diapMax

        -----------------------------------------------------------------------
        -- valuators

        -- FLcount
        drawCount diap step1 mStep2 _ = f "FLcount" $
            [ imin diap, imax diap
            , double step1, double step2
            , int itype ] 
            ++ frame ++ [noOpc]
            where (step2, itype) = case mStep2 of
                    -- type 1 FLcount with 2 steps
                    Just n  -> (n, 1)
                    -- type 2 FLcount with a single step
                    Nothing -> (step1, 2)

        -- FLjoy
        drawJoy (Span dX sX) (Span dY sY) _ = f "FLjoy" $
            [ imin dX, imax dX, imin dY, imax dY
            , getScale sX, getScale sY
            , noDisp, noDisp 
            ] ++ frame ++ [noOpc]

        -- FLknob
        drawKnob span _ = f "FLknob" $ 
            drawSpan span ++ [getKnobType ctx, noDisp] 
            ++ frame ++ getKnobCursorSize ctx            

        -- FLroller
        drawRoller (Span d s) step _ = f "FLroller" $
            [ imin d, imax d, double step
            , getScale s, getRollerType (getDefOrient rect) ctx, noDisp
            ] ++ frame

        -- FLslider
        drawSlider span _ = f "FLslider" $ 
            drawSpan span 
            ++ [getSliderType (getDefOrient rect) ctx, noDisp] 
            ++ frame

        -- FLtext
        drawText diap step _ = f "FLtext" $ 
            [imin diap, imax diap, double step, getTextType ctx] ++ frame

        -----------------------------------------------------------------------
        -- other widgets
        
        -- FLbox
        drawBox text = (frameBox P.$$ ) $ 
            P.vcat $ fmap (uncurry phi) $ splitText rect text
            where phi r label = fWithLabel label "FLbox" $ 
                    [ int 1, getFontType ctx, getFontSize ctx] ++ frameBy r

                  frameBox = fWithLabel "" "FLbox" $
                    [ getBoxType ctx, int 1, int 1 ] ++ frame

                  splitText :: Rect -> String -> [(Rect, String)]
                  splitText = undefined
    
        -- FLbutBank
        drawButBank xn yn = f "FLbutBank" $ 
            [getButtonType ctx, int xn, int yn] ++ frame ++ [noOpc] 

        -- FLbutton
        drawButton = f "FLbutton" $ [int 1, int 0, getButtonType ctx] ++ frame ++ [noOpc]

        -- FLvalue
        drawValue _ = f "FLvalue" frame 

        -- FLvkeybd
        drawVkeybd = fWithLabel "" "FLvkeybd" frame  

-----------------------------------------------------------
-- cascading context, here we group properties by type

data PropCtx = PropCtx 
    { ctxBorder       :: Maybe BorderType
    , ctxLabel        :: Maybe String
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
                  Nothing Nothing
   
setPropCtx :: Prop -> PropCtx -> PropCtx 
setPropCtx p x = case p of
            SetBorder       a -> x { ctxBorder = Just a }
            SetLabel        a -> x { ctxLabel  = Just a }
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

------------------------------------------------------------------
-- Converting readable properties to integer codes

maybeDef :: Default a => Maybe a -> a
maybeDef = maybe def id

intProp :: Default a => (PropCtx -> Maybe a) -> (a -> Int) -> (PropCtx -> Doc)
intProp select convert = int . convert . maybeDef . select 

getScale :: ScaleType -> Doc
getScale x = int $ case x of
    Linear      -> 0
    Exponential -> -1

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
getSliderType defOrient ctx = int $ 
    case (getOrient defOrient ctx, maybeDef $ ctxSliderType ctx) of
        (Hor, Fill)         -> 1
        (Ver, Fill)         -> 2
        (Hor, Engraved)     -> 3
        (Ver, Engraved)     -> 4
        (Hor, Nice)         -> 5
        (Ver, Nice)         -> 6
        (Hor, Upbox)        -> 7
        (Ver, Upbox)        -> 8

getTextType :: PropCtx -> Doc
getTextType = intProp ctxTextType $ \x -> case x of  
    NormalText  -> 1
    NoDrag      -> 2
    NoEdit      -> 3

getBoxType :: PropCtx -> Doc
getBoxType = intProp ctxBoxType $ succ . fromEnum

getFontSize :: PropCtx -> Doc 
getFontSize ctx = int $ maybe defFontSize id $ ctxFontSize ctx

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
getButtonType = intProp ctxButtonType $ \x -> 
    onPlastic (isPlastic x) $ case buttonTypeVal x of
        NormalButton    -> 1
        LightButton     -> 2
        CheckButton     -> 3
        RoundButton     -> 4   
    where onPlastic x 
            | x         = ( + 20)
            | otherwise = id

