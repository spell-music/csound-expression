{-# Language 
    TypeSynonymInstances, 
    FlexibleInstances, 
    MultiParamTypeClasses, 
    FlexibleContexts, 
    TypeFamilies #-}
-- | GUI (Graphical User Interface) elements are handy to change 
-- the parameters of the sound in real time. It includes sliders, 
-- knobs, rollers, buttons and other widgets. 
--
-- A GUI element consists of two parts. They are view (how it looks)
-- and logic (what's going on with it). For example a slider can be
-- horizontal or vertical or green or yellow or small or big. It's the view 
-- of the slider. And a slider can produce a continuous signal within the 
-- given interval. It's a logic of the slider. 
--
-- Let's talk about the view. The view is divided on two parts:
--
-- * where element is placed or Layout. 
--
-- * all other  properties or just Properties. 
--
-- The layout is defined with very simple functions. There are vertical and horizontal grouping 
-- of the elements. We can scale the element within the group and include an empty
-- space in the group. Everything is aligned (see "Csound.Gui.Layout"). 
-- Other properties include colors, fonts (size and type), borders, specific properties 
-- of the widgets (see "Csound.Gui.Props").
--
-- Let's consider the logic. The logic consists of three parts:
--
-- * what is consumed ('Csound.Gui.Output')
--
-- * what is produced ('Csound.Gui.Input')
--
-- * what's going on inside ('Csound.Gui.Inner')
--
-- A widget can react on values, produce values or do something useful. 
-- There are special types of widgets:
--
-- * 'Csound.Gui.Source'  - they produce values only
--
-- * 'Csound.Gui.Sink'    - they consume values only
--
-- * 'Csound.Gui.Display' - something is going on inside them (for example, it can show a "hello world" message)
-- 
--
-- Widgets can be simple and compound. Simple widgets are primitive elements
-- (sliders, knobs, rollers, buttons). We have a special constructors that 
-- produce simple widgets (see "Csound.Gui.Widget"). Compound widgets glue together
-- several widgets. That is the view contains several elements and all of them 
-- involved in the logic of the widget.
--
--
module Csound.Control.Gui (
    -- * Gui
    Gui, 
    Widget, Input, Output, Inner,
    Sink, Source, Display, SinkSource,
    widget, sink, source, display, sinkSource, sinkSlice, sourceSlice,
    mapSource, mapGuiSource, 
    mhor, mver, msca,
    joinSource,

    -- * Panels
    panel, win, panels, panelBy,
    keyPanel, keyWin, keyPanels, keyPanelBy,

    -- * Re-exports
    module Csound.Control.Gui.Layout,
    module Csound.Control.Gui.Props,
    module Csound.Control.Gui.Widget,

    -- * Lifters
    -- | An easy way to combine visuals for sound sources.
    hlifts, vlifts,

    lift1, hlift2, vlift2, hlift3, vlift3, hlift4, vlift4, hlift5, vlift5,

    -- ** Lifters with visual scaling
    hlifts', vlifts',

    hlift2', vlift2', hlift3', vlift3', hlift4', vlift4', hlift5', vlift5'
) where

import Csound.Typed

import Csound.Typed.Gui

import Csound.Control.Gui.Layout
import Csound.Control.Gui.Props
import Csound.Control.Gui.Widget
import Csound.SigSpace

instance SigSpace a => SigSpace (Source a) where
    mapSig f = mapSource (mapSig f)

instance (At Sig (SE Sig) a) => At Sig (SE Sig) (Source a) where
    type AtOut Sig (SE Sig) (Source a) = Source (AtOut Sig (SE Sig) a)
    at f a = mapSource (at f) a

instance (At Sig2 Sig2 a) => At Sig2 Sig2 (Source a) where
    type AtOut Sig2 Sig2 (Source a) = Source (AtOut Sig2 Sig2 a)
    at f a = mapSource (at f) a

instance (At Sig2 (SE Sig2) a) => At Sig2 (SE Sig2) (Source a) where
    type AtOut Sig2 (SE Sig2) (Source a) = Source (AtOut Sig2 (SE Sig2) a)
    at f a = mapSource (at f) a

-- | Creates a window with the given name, size and content
--
-- > win name (width, height) gui
win :: String -> (Int, Int) -> Gui -> SE ()
win name (x, y) = panelBy name (Just $ Rect 0 0 x y)

keyWin :: String -> (Int, Int) -> Gui -> SE ()
keyWin name (x, y) = keyPanelBy name (Just $ Rect 0 0 x y)

-- | Hides the SE inside Source.
joinSource :: Source (SE a) -> Source a
joinSource a = do
    (g, mv) <- a
    v <- mv
    return (g, v)

----------------------------------------------------------------------------------
-- easy grouppings for GUIs

-- | Groups a list of Source-widgets. The visuals are horizontally aligned.
hlifts :: ([a] -> b) -> [Source a] -> Source b
hlifts = genLifts hor

-- | Groups a list of Source-widgets. The visuals are vertically aligned.
vlifts :: ([a] -> b) -> [Source a] -> Source b
vlifts = genLifts ver

-- | Groups a list of Source-widgets. The visuals are horizontally aligned. 
-- It uses the list of proportions.
hlifts' :: [Double] -> ([a] -> b) -> [Source a] -> Source b
hlifts' props = genLifts (applyProportionsToList props hor)

-- | Groups a list of Source-widgets. The visuals are vertically aligned.
-- It uses the list of proportions.
vlifts' :: [Double] -> ([a] -> b) -> [Source a] -> Source b
vlifts' props = genLifts (applyProportionsToList props ver)

applyProportionsToList :: [Double] -> ([Gui] -> Gui) -> [Gui] -> Gui
applyProportionsToList props f as = f $ zipWith sca (props ++ repeat 1) as

genLifts :: ([Gui] -> Gui) -> ([a] -> b) -> [Source a] -> Source b
genLifts gf f as = fmap phi $ sequence as
    where 
        phi xs = (gf gs, f vs)
            where (gs, vs) = unzip xs


-- | The shortcut for @mapSource@.
lift1 :: (a -> b) -> Source a -> Source b
lift1 = mapSource

lift2 :: (Gui -> Gui -> Gui) -> (a -> b -> c) -> Source a -> Source b -> Source c
lift2 gf f ma mb = source $ do
    (ga, a) <- ma
    (gb, b) <- mb
    return $ (gf ga gb, f a b)

lift2' a b gf = lift2 (tfm2 a b gf)
    where tfm2 sa sb gf = \a b -> gf (sca sa a) (sca sb b)

-- | Combines two sound sources. Visuals are aligned horizontally
-- and the sound sources a grouped with the given function. 
hlift2 :: (a -> b -> c) -> Source a -> Source b -> Source c
hlift2 = lift2 (\a b -> hor [a, b])

-- | Combines two sound sources. Visuals are aligned vertically
-- and the sound sources a grouped with the given function. 
vlift2 :: (a -> b -> c) -> Source a -> Source b -> Source c
vlift2 = lift2 (\a b -> ver [a, b])

-- | It's just like the @hlift2@ but two more parameters change visual scaling of the widgets.
hlift2' :: Double -> Double -> (a -> b -> c) -> Source a -> Source b -> Source c
hlift2' sa sb = lift2' sa sb (\a b -> hor [a, b])

-- | It's just like the @vlift2@ but two more parameters change visual scaling of the widgets.
vlift2' :: Double -> Double -> (a -> b -> c) -> Source a -> Source b -> Source c
vlift2' sa sb = lift2' sa sb (\a b -> ver [a, b])

lift3 :: (Gui -> Gui -> Gui -> Gui) -> (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
lift3 gf f ma mb mc = source $ do
    (ga, a) <- ma
    (gb, b) <- mb
    (gc, c) <- mc
    return $ (gf ga gb gc, f a b c)

lift3' sa sb sc gf = lift3 (tfm3 sa sb sc gf)
    where tfm3 sa sb sc gf = \a b c -> gf (sca sa a) (sca sb b) (sca sc c)

-- | The same as @hlift2@ but for three sound sources.
hlift3 :: (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
hlift3 = lift3 (\a b c -> hor [a, b, c])

-- | The same as @vlift2@ but for three sound sources.
vlift3 :: (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
vlift3 = lift3 (\a b c -> ver [a, b, c])

-- | The same as @hlift2'@ but for three sound sources.
hlift3' :: Double -> Double -> Double -> (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
hlift3' a b c = lift3' a b c (\a b c -> hor [a, b, c])

-- | The same as @vlift2'@ but for three sound sources.
vlift3' :: Double -> Double -> Double -> (a -> b -> c -> d) -> Source a -> Source b -> Source c -> Source d
vlift3' a b c = lift3' a b c (\a b c -> ver [a, b, c])

lift4 :: (Gui -> Gui -> Gui -> Gui -> Gui) -> (a -> b -> c -> d -> e) -> Source a -> Source b -> Source c -> Source d -> Source e
lift4 gf f ma mb mc md = source $ do
    (ga, a) <- ma
    (gb, b) <- mb
    (gc, c) <- mc
    (gd, d) <- md
    return $ (gf ga gb gc gd, f a b c d)

lift4' sa sb sc sd gf = lift4 (tfm3 sa sb sc sd gf)
    where tfm3 sa sb sc sd gf = \a b c d -> gf (sca sa a) (sca sb b) (sca sc c) (sca sd d)

-- | The same as @hlift2@ but for four sound sources.
hlift4 :: (a -> b -> c -> d -> e) -> Source a -> Source b -> Source c -> Source d -> Source e
hlift4 = lift4 (\a b c d -> hor [a, b, c, d])

-- | The same as @vlift2@ but for four sound sources.
vlift4 :: (a -> b -> c -> d -> e) -> Source a -> Source b -> Source c -> Source d -> Source e
vlift4 = lift4 (\a b c d -> ver [a, b, c, d])

-- | The same as @hlift2'@ but for four sound sources.
hlift4' :: Double -> Double -> Double -> Double -> (a -> b -> c -> d -> e) -> Source a -> Source b -> Source c -> Source d -> Source e
hlift4' a b c d = lift4' a b c d (\a b c d -> hor [a, b, c, d])

-- | The same as @vlift2'@ but for four sound sources.
vlift4' :: Double -> Double -> Double -> Double -> (a -> b -> c -> d -> e) -> Source a -> Source b -> Source c -> Source d -> Source e
vlift4' a b c d = lift4' a b c d (\a b c d -> ver [a, b, c, d])


lift5 :: (Gui -> Gui -> Gui -> Gui -> Gui -> Gui) -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Source a1 -> Source a2 -> Source a3 -> Source a4 -> Source a5 -> Source b
lift5 gf f ma1 ma2 ma3 ma4 ma5 = source $ do
    (ga1, a1) <- ma1
    (ga2, a2) <- ma2
    (ga3, a3) <- ma3
    (ga4, a4) <- ma4
    (ga5, a5) <- ma5
    return $ (gf ga1 ga2 ga3 ga4 ga5, f a1 a2 a3 a4 a5)

lift5' sa sb sc sd se gf = lift5 (tfm3 sa sb sc sd se gf)
    where tfm3 sa sb sc sd se gf = \a b c d e -> gf (sca sa a) (sca sb b) (sca sc c) (sca sd d) (sca se e)

-- | The same as @hlift2@ but for five sound sources.
hlift5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Source a1 -> Source a2 -> Source a3 -> Source a4 -> Source a5 -> Source b
hlift5 = lift5 (\a b c d e -> hor [a, b, c, d, e])

-- | The same as @vlift2@ but for five sound sources.
vlift5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Source a1 -> Source a2 -> Source a3 -> Source a4 -> Source a5 -> Source b
vlift5 = lift5 (\a b c d e -> ver [a, b, c, d, e])

-- | The same as @hlift2'@ but for five sound sources.
hlift5' :: Double -> Double -> Double -> Double -> Double -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Source a1 -> Source a2 -> Source a3 -> Source a4 -> Source a5 -> Source b
hlift5' a b c d e = lift5' a b c d e (\a b c d e -> hor [a, b, c, d, e])

-- | The same as @vlift2'@ but for five sound sources.
vlift5' :: Double -> Double -> Double -> Double -> Double -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> Source a1 -> Source a2 -> Source a3 -> Source a4 -> Source a5 -> Source b
vlift5' a b c d e = lift5' a b c d e (\a b c d e -> ver [a, b, c, d, e])

