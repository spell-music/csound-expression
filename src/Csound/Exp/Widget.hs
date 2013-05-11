module Csound.Exp.Widget where

import Control.Applicative

import Csound.Exp.Gui

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.Logic
import Csound.Exp.Event

import Csound.Opcode(idur, linseg)

change :: Sig -> BoolSig
change = undefined

mkGuiVar :: Int -> Var
mkGuiVar n = Var GlobalVar Kr ("fl_" ++ show n)

mkGuiHandle :: Int -> Var
mkGuiHandle n = Var GlobalVar Ir ("hfl_" ++ show n)

type Reader a = SE a
type Writer a = a -> SE ()
type Inner    = SE ()

noWrite :: Writer ()
noWrite = return 

noRead :: Reader ()
noRead  = return ()

noInner :: Inner
noInner = return ()

newtype Widget a b = Widget { unWidget :: SE (Gui, Writer a, Reader b, Inner) }

type Sink   a = Widget a ()
type Source a = Widget () a
type Display  = Widget () ()

widget :: Widget a b -> SE (Gui, Writer a, Reader b)
widget a = do 
    (gui, writer, reader, inner) <- unWidget a
    appendToGui gui inner
    return (gui, writer, reader)

sink :: Widget a b -> SE (Gui, Writer a)
sink a = do
    (gui, writer, _) <- widget a
    return (gui, writer)

source :: Widget a b -> SE (Gui, Reader b)
source a = do
    (gui, _, reader) <- widget a
    return (gui, reader)

mkWidgetWith :: SE (Gui, Writer a, Reader b, Inner) -> Widget a b
mkWidgetWith = Widget

mkDisplayWith :: SE (Gui, Inner) -> Display 
mkDisplayWith a = mkWidgetWith $ do
    (gui, inner) <- a
    return (gui, noWrite, noRead, inner)
    
mkWidget :: SE (Gui, Writer a, Reader b) -> Widget a b
mkWidget = Widget . fmap appendEmptyBody
    where appendEmptyBody (a, b, c) = (a, b, c, noInner)

mkSink :: SE (Gui, Writer a) -> Sink a
mkSink a = mkWidget $ do
    (gui, writer) <- a 
    return (gui, writer, noRead)

mkSource :: SE (Gui, Reader b) -> Source b
mkSource a = mkWidget $ do
    (gui, reader) <- a 
    return (gui, noWrite, reader)
    
mkDisplay :: SE Gui -> Display
mkDisplay a = mkWidget $ do
    gui <- a
    return (gui, noWrite, noRead)

-----------------------------------------------------------------------------  
-- primitive elements

slider :: Label -> Widget Sig Sig
slider label = mkWidget $ do
    name <- newGuiId
    let gui = Prim name label Slider
        var = mkGuiVar name
        writer = writeVar var
        reader = return $ readVar var
    return (gui, writer, reader)

btn :: Label -> Source (Event ()) 
btn label = mkSource $ do
    name <- newGuiId 
    let gui = Prim name label Btn
        var = mkGuiVar name
        reader = return $ trigger $ change $ readVar var
    return (gui, reader)

text :: Display 
text = mkDisplay $ do
    name <- newGuiId
    return $ Prim name "" Text

------------------------------------------------------------------------------------

linenWidget :: Source Sig
linenWidget = mkSource $ do
    (g1, r1) <- source $ slider "first"
    (g2, r2) <- source $ slider "second"
    let out = liftA2 fun r1 r2        
    return (Comp [g1, g2], out)
    where fun a b = linseg [0, ir a, 1, idur - ir a - ir b, 1, ir b, 0]

adder :: Display
adder = mkDisplayWith $ do
    (ga, ina)   <- source $ slider "a"
    (gb, inb)   <- source $ slider "b"
    (gres, res) <- sink   $ slider "res"
    return (Comp [ga, gb, gres], 
            res =<< liftA2 (+) ina inb)

