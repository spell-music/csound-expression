module Main where

import Csound.Base
import Csound.Sam

main = main4

a1 = infSig1 $ osc 220
a2 = infSig1 $ osc 330
a3 = infSig1 $ osc 440

main1 = dac $ do
	(g, sam) <- tog 4 [("220", a1), ("330", a2)]
	panel g
	mul 0.5 $ runSam 120 sam

-----------------------------

b1 = infSig1 $ sqr 220
b2 = infSig1 $ sqr 330
b3 = infSig1 $ sqr 440

c1 = infSig1 $ tri 220
c2 = infSig1 $ tri 330
c3 = infSig1 $ tri 440

main2 = dac $ do
	(g, sam) <- live 4 ["triangle", "square"]
		[ c1, b1
		, c2, b3
		, c3, b3]
	panel g
	mul 0.3 $ runSam 120 sam

-----------------------------

main3 = dac $ do
	(g, res) <- mixer $ fmap (\x -> mixMono (show x) (osc $ sig $ int x)) [220, 330, 440]
	win "mixer" (600, 300) g
	return $ mul 0.5 $ res

-----------------------------

run = runSam 120

main4 = dac $ do
	(g1, sam1) <- tog 4 [("220", a1), ("330", a2)]
	(g2, sam2) <- sim 4 [("220", a1), ("330", a2)]
	(g3, res)  <- mixer [("tog", run sam1), ("sim", run sam2)]
	win "main" (600, 400) $ ver [sca 0.6 $ hor [g1, g2], g3]
	return res

-----------------------------

main5 = dac $ do
	(gui, fx) <- fxHor
		[ uiFilter False 0.5 0.5 0.5
		, uiChorus False 0.5 0.5 0.5 0.5
		, uiPhaser False 0.5 0.5 0.5 0.5
		, uiReverb True  0.5 0.5
		, uiGain   0.5
		]
	win "main" (900, 400) gui
	fx $ fromMono $ saw 110
