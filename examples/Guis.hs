module Main where

import Control.Applicative

import Csound.Base

linenWidget :: Source Sig
linenWidget = mkSource $ do
    (g1, r1) <- source $ slider "rise time"
    (g2, r2) <- source $ slider "decay time"
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

main = writeCsd "tmp.csd" $ do
    g <- display adder
    runFl g


