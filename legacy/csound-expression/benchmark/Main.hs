module Main where

import Gauge
import Gauge.Main (defaultMain)

import Csound.Base

main :: IO ()
main =
  defaultMain
    [ bgroup "drone tests" $ zipWith forDrone [1..] [testDrone, testDrone2, testDrone3]
    ]
  where
    forDrone n fun = bench ("drone: " <> show n) $ nfAppIO (renderCsd . fun) 100
