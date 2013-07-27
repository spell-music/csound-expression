module New where

type Name = String
type Chn = Int
type Time = Double
type Dur = Double

data Prim 
    = P Int 
    | Var Name
    | PrimInt Int | PrimDouble Double | PrimString String 
    | PrimFtable Ftable 

newtype Mu f = Mu { unMu :: f (Mu f) }

data Exp a 
    = K Prim
    | If a a a
    | Tfm Name [Prim] [a]
    | Sco [Note] a
    | Select Int a
    | ExpSlider Slider
    | ExpMouse Mouse
    | ExpMidi Midi a 
    | Toggle [(Button, [a])]
    | ExpRadio [a]
    | Seq a a
    | Metro [a]


data Layout a
    = Group [a]
    | Hor [a]
    | Ver [a]

-----------------------------------------
-- dsp types

newtype Sig = Sig (Mu Exp)
newtype Int' = Int' (Mu Exp)
newtype Double' = Double' (Mu Exp)
data Ftable = Ftable [Double]
newtype Se = Se (Mu Exp)
data Note = Note Time Dur [Prim]

-----------------------------------------
-- gui types

data Diap = Diap Double Double

data Slider = Slider Diap Name
data Button = Button Name
data Radio  = Radio Name 

----------------------------------------
-- ui

data Key    = Key Char
data Mouse  = Mouse
data Midi   = Midi Chn


---
un = undefined

osc :: Ftable -> Sig -> Sig
osc = un

linseg :: [Double'] -> Sig
linseg = un

---------------------

class View a where

instance View Slider
instance View Button
instance View Text

view :: View a => a -> View

hcat :: [View] -> View
vcat :: [View] -> View

mcat :: Int -> [View] -> View

scale :: Double -> View -> View

