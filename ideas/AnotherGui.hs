


data Widget ins outs = ...

type Source a = Widget () a
type Sink   a = Widget a ()

type Trig = Event ()

slider :: Widget Sig Sig
butn   :: Widget Trig Trig

widget :: SE (Gui, ins, outs) -> Widget ins outs

parts   :: Widget ins outs -> SE (Gui, ins, outs, loop)

inputs  :: Widget ins outs -> SE ins
outputs :: Widget ins outs -> SE outs
gui     :: Widget ins outs -> SE Gui


source  :: Source a -> SE (Gui, a)
sink    :: Sink   a -> SE (Gui, a)

-----------------------------------
--

adsr :: Source Sig
adsr = widget $ do
    (gs, as) <- fmap unzip $ mapM (source . knob) $ words "att dec rel sus" 
    return (hcat gs, (), toAdsr as) 
    where toAdsr [a,b,c,d] = linseg a ...

sliderBnk :: Int -> Source [Sig]
sliderBnk = ...

