-- | Rendering of Csound files and playing the music in real time.
--
-- How are we going to get the sound out of Haskell code? 
-- Instruments are ready and we have written all the scores for them. 
-- Now, it's time to use the rendering functions. We can render haskell expressions
-- to Csound code. A rendering function takes a value that represents a sound ('Csound.Control.GE')
-- and produces a string with Csound code. It can take a value that represents 
-- the flags for the csound compiler and global settings ('Csound.IO.CsdOptions'). 
-- Then we can save this string to file and convert it to sound with csound compiler
--
-- > csound -o music.wav music.csd
--
-- Or we can play it in real time with -odac flag. It sends the sound directly to
-- soundcard. It's usefull when we are using midi or tweek the parameters in real time
-- with sliders or knobs.
--
-- > csound -odac music.csd
--
-- The main function of this module is 'Csound.IO.renderCsdBy'. Other function are nothing but
-- wrappers that produce the Csound code and make something useful with it (saving to file,
-- playing with specific player or in real time).  
module Csound.IO (    

    -- * Options
    -- | We can set some csound options.
    Channel, CtrlId, CsdOptions(..), module Data.Default,

    -- * Rendering
    renderCsd, renderCsdBy, 
    writeCsd, writeCsdBy, 
    
    -- * Playing the sound
    playCsd, playCsdBy, 
    mplayer, mplayerBy, totem, totemBy,

    -- * Live performance
    dac, dacBy,
) where


import System.Cmd(system)
import Data.Default

import Csound.Exp.GE(GE)
import Csound.Exp.Options
import Csound.Render(render)
import Csound.Exp.Tuple(Out)

-- | Renders Csound file.
renderCsd :: (Out a) => GE a -> IO String
renderCsd = renderCsdBy def

-- | Renders Csound file with options.
renderCsdBy :: (Out a) => CsdOptions -> GE a -> IO String
renderCsdBy opt as = render opt as

-- | Render Csound file and save it to the give file.
writeCsd :: (Out a) => String -> GE a -> IO ()
writeCsd file sco = writeFile file =<< renderCsd sco 

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: (Out a) => CsdOptions -> String -> GE a -> IO ()
writeCsdBy opt file sco = writeFile file =<< renderCsdBy opt sco

-- | Renders Csound file, saves it to the given file, renders with csound command and plays it with the given program.
-- 
-- > playCsd program file sco 
--
-- Produces files @file.csd@ (with 'Csound.Render.Mix.renderCsd') and @file.wav@ (with @csound@) and then invokes:
--
-- > program "file.wav"
playCsd :: (Out a) => (String -> IO ()) -> String -> GE a -> IO ()
playCsd = playCsdBy def

-- | Works just like 'Csound.Render.Mix.playCsd' but you can supply csound options.
playCsdBy :: (Out a) => CsdOptions -> (String -> IO ()) -> String -> GE a -> IO ()
playCsdBy opt player file sco = do
    writeCsdBy opt fileCsd sco
    _ <- system $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    player fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

simplePlayCsdBy :: (Out a) => CsdOptions -> String -> String -> GE a -> IO ()
simplePlayCsdBy opt player = playCsdBy opt phi
    where phi file = do
            _ <- system $ player ++ " " ++ file
            return ()
            

-- | Renders csound code to file @tmp.csd@ and plays it with @-odac@ option
-- (sound output goes to soundcard in real time).
dac :: (Out a) => GE a -> IO ()
dac = dacBy def

-- | 'Csound.Base.dac' with options.
dacBy :: (Out a) => CsdOptions -> GE a -> IO ()
dacBy opt sco = do
    writeCsdBy opt "tmp.csd" sco
    _ <- system $ "csound -odac " ++ "tmp.csd" 
    return ()

--------------------------------------------------------
-- players

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayer :: (Out a) => GE a -> IO ()
mplayer = mplayerBy def

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayerBy :: (Out a) => CsdOptions -> GE a -> IO ()
mplayerBy opt = simplePlayCsdBy opt "mplayer" "tmp"

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totem :: (Out a) => GE a -> IO ()
totem = totemBy def

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totemBy :: (Out a) => CsdOptions -> GE a -> IO ()
totemBy opt = simplePlayCsdBy opt "totem" "tmp"

