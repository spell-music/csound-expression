module Main where

import Control.Applicative

import Csound.Base

unit = Span (Diap 0 1) Linear

linenWidget :: Source Sig
linenWidget = mkSource $ do
    (g1, r1) <- slider unit 0.5
    (g2, r2) <- slider unit 0.5
    let out = liftA2 fun r1 r2        
    return (ver [g1, g2], out)
    where fun a b = linseg [0, a', 1, idur - a' - b', 1, b', 0]
                where a' = idur * ir a
                      b' = idur * ir b  
{-
adder :: Display
adder = mkDisplayWith $ do
    (ga, ina)   <- slider "a"
    (gb, inb)   <- slider "b"
    (gres, res) <- value "res"
    return (Comp [ga, gb, gres], 
            res =<< liftA2 (+) ina inb)
-}


instr :: SE Sig -> D -> SE Sig
instr env cps = do
    e <- env
    return $ 0.5 * e * osc (sig cps)

pureTone () = 0.7 * osc 440 
    
u = fmap (props [SetSliderType Fill] . fst) $ slider unit 0.5
v = fmap fst $ knob unit 0.5
r = fmap fst $ roller unit 0.001 0.01
c1 = fmap fst $ count (Diap 0 10) 1 Nothing 1
c2 = fmap fst $ count (Diap 0 10) 1 (Just 2) 1
b = fmap (setLabel "push me" . fst) button
bb = fmap fst $ butBank 5 3

d = fmap (setBoxType BorderBox) $ box $ t ++ t ++ t
    where t = " Hello everybody, let's look at some cool text in the box "

main = dac $ do
    g1 <- u
    g2 <- u
    g3 <- u
    g4 <- r
    g5 <- c1 
    g6 <- c2
    g7 <- b
    g8 <- bb
    g9 <- d

--    let g = ver [sca 0.5 $ setLabel "hi there" g1, hor [g9, g7, sca 2 g9], hor [setLabel "common" g1, g9, g8]]
    let g = hor [ver [g9, g9, g8, g4], g9, g9, ver [g9, g9]]
    
    runFl g
    return ()
--    schedule (instr env) $ fmap (const (1, 440)) $ metroE (1/5)

