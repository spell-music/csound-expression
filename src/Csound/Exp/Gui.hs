module Csound.Exp.Gui where

import qualified Data.IntMap as IM

import Text.PrettyPrint.Leijen
import qualified Csound.Render.Pretty as P

import Csound.Exp hiding (P)

data Rect = Rect
    { rectX     :: Int
    , rectY     :: Int
    , rectW     :: Int
    , rectH     :: Int }

newtype GuiHandle = GuiHandle { unGuiHandle :: Int }

data Win = Win 
    { winTitle :: String 
    , winRect  :: Rect
    , winGui   :: Gui }

data GuiNode = GuiNode
    { guiNodeElem   :: Gui
    , guiNodeHandle :: GuiHandle }

data Gui
    = Comp [Gui] 
    | Prim P 
    | GuiVar GuiHandle

type ElemOut = [Var]

data P = P ElemOut Label Elem

sliderElem as x = Prim (P as x Slider)
btnElem as x = Prim (P as x Btn)
textElem as x = Prim (P as x Text)

data Elem = Slider | Btn | Radio | Text

type Label = String

type GuiMap = IM.IntMap Gui

guiMap :: [GuiNode] -> GuiMap
guiMap = IM.fromList . fmap (\(GuiNode elem (GuiHandle n)) -> (n, elem))

restoreTree :: GuiMap -> Gui -> Gui
restoreTree m x = case x of
    Comp as     -> Comp $ fmap rec as
    GuiVar h    -> rec $ m IM.! unGuiHandle h
    Prim elem   -> Prim elem
    where rec = restoreTree m


drawGui :: Win -> Doc
drawGui w = onPanel (winTitle w) (winRect w) $ 
    vcat $ fmap (uncurry drawPrim) $ withRects (winRect w) $ 
    flattenGui (winGui w)
    where
        onPanel title rect body = vcat [
            P.ppProc "FLpanel" [text $ show title, int $ rectW rect, int $ rectH rect],
            body,
            P.ppProc "FLpanelEnd" []]

        drawPrim r (P outs label elem) = case elem of
            Slider  -> f "FLslider" $ fmap int $ [0, 1, 0, 5, -1] ++ frame 
            Btn     -> f "FLbutton" $ fmap int $ [0, 0, 1] ++ frame 
            Text    -> f "FLvalue" []
            where f name args = P.ppMoOpc (fmap P.ppVar outs) name ((text $ show label) : args)
                  frame = [rectW r, rectH r, rectX r, rectY r]

        flattenGui x = case x of
            Comp xs     -> concat $ fmap flattenGui xs
            Prim elem   -> [elem]
            _           -> error "Csound.Exp.Gui.drawGui: orphan handle"

        withRects rect xs = zip (fmap mkRect [0 .. pred $ length xs]) xs
            where
                dh = div (rectH rect) (length xs)
                mkRect num = Rect 0 (num * dh) (rectW rect) dh




