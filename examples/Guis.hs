module Main where

import Control.Applicative

import Csound.Base

linenWidget :: Source Sig
linenWidget = mkSource $ do
    (g1, r1) <- source $ slider "rise time"
    (g2, r2) <- source $ slider "decay time"
    let out = liftA2 fun r1 r2        
    return (Comp [g1, g2], out)
    where fun a b = linseg [0, a', 1, idur - a' - b', 1, b', 0]
                where a' = idur * ir a
                      b' = idur * ir b  

adder :: Display
adder = mkDisplayWith $ do
    (ga, ina)   <- source $ slider "a"
    (gb, inb)   <- source $ slider "b"
    (gres, res) <- sink   $ text "res"
    return (Comp [ga, gb, gres], 
            res =<< liftA2 (+) ina inb)

instr :: SE Sig -> D -> SE Sig
instr env cps = do
    e <- env
    return $ 0.5 * e * osc (sig cps)

pureTone () = 0.7 * osc 440 


main = writeCsd "tmp.csd" $ do
    (g1, vol) <- source $ slider "vol"    
    (g2, wr)  <- sink $ text "val"
    res <- schedule pureTone $ fmap (\x -> (0.5, x))$ (metroE 1)
    
    g <- display $ mkDisplayWith $ return (Comp [g1, g2], everyNth 0.4 wr res) 
    runFl g

    return $ fmap (res * ) vol 


everyNth d wr sig = runEvt (metroE d) $ const $ wr sig
