-- | Rendering of Csound files and playing the music in real time.
--
-- How are we going to get the sound out of Haskell code? 
-- Instruments are ready and we have written all the scores for them. 
-- Now, it's time to use the rendering functions. We can render haskell expressions
-- to Csound code. A rendering function takes a value that represents a sound ('Csound.Control.GE')
-- and produces a string with Csound code. It can take a value that represents 
-- the flags for the csound compiler and global settings ('Csound.IO.Options'). 
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
    -- * Rendering
    renderCsd, renderCsdBy, 
    writeCsd, writeCsdBy, 
    
    -- * Playing the sound
    playCsd, playCsdBy, 
    mplayer, mplayerBy, totem, totemBy,

    -- * Live performance
    dac, dacBy,
   
    -- * Render procedures
    renderCsd_, renderCsdBy_, 
    writeCsd_, writeCsdBy_, 
    dac_, dacBy_
) where

import System.Cmd(system)
import Data.Default
import Csound.Typed

-- | Renders Csound file.
renderCsd :: (Sigs a) => SE a -> IO String
renderCsd = renderOut

-- | Renders Csound file with options.
renderCsdBy :: (Sigs a) => Options -> SE a -> IO String
renderCsdBy opt as = renderOutBy opt as

-- | Render Csound file and save it to the give file.
writeCsd :: (Sigs a) => String -> SE a -> IO ()
writeCsd file csd = writeFile file =<< renderCsd csd

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: (Sigs a) => Options -> String -> SE a -> IO ()
writeCsdBy opt file csd = writeFile file =<< renderCsdBy opt csd

-- | Renders Csound file, saves it to the given file, renders with csound command and plays it with the given program.
-- 
-- > playCsd program file csd 
--
-- Produces files @file.csd@ (with 'Csound.Render.Mix.renderCsd') and @file.wav@ (with @csound@) and then invokes:
--
-- > program "file.wav"
playCsd :: (Sigs a) => (String -> IO ()) -> String -> SE a -> IO ()
playCsd = playCsdBy def

-- | Works just like 'Csound.Render.Mix.playCsd' but you can supply csound options.
playCsdBy :: (Sigs a) => Options -> (String -> IO ()) -> String -> SE a -> IO ()
playCsdBy opt player file csd = do
    writeCsdBy opt fileCsd csd
    _ <- system $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    player fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

simplePlayCsdBy :: (Sigs a) => Options -> String -> String -> SE a -> IO ()
simplePlayCsdBy opt player = playCsdBy opt phi
    where phi file = do
            _ <- system $ player ++ " " ++ file
            return ()
            

-- | Renders csound code to file @tmp.csd@ and plays it with @-odac@ option
-- (sound output goes to soundcard in real time).
dac :: (Sigs a) => SE a -> IO ()
dac = dacBy def

-- | 'Csound.Base.dac' with options.
dacBy :: (Sigs a) => Options -> SE a -> IO ()
dacBy opt csd = do
    writeCsdBy opt "tmp.csd" csd
    _ <- system $ "csound -odac " ++ "tmp.csd" 
    return ()

--------------------------------------------------------
-- players

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayer :: (Sigs a) => SE a -> IO ()
mplayer = mplayerBy def

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayerBy :: (Sigs a) => Options -> SE a -> IO ()
mplayerBy opt = simplePlayCsdBy opt "mplayer" "tmp"

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totem :: (Sigs a) => SE a -> IO ()
totem = totemBy def

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totemBy :: (Sigs a) => Options -> SE a -> IO ()
totemBy opt = simplePlayCsdBy opt "totem" "tmp"

-----------------------------------------------------------------------------
--
-- | Renders Csound file.
renderCsd_ :: SE () -> IO String
renderCsd_ = renderOut_

-- | Renders Csound file with options.
renderCsdBy_ :: Options -> SE () -> IO String
renderCsdBy_ opt as = renderOutBy_ opt as

-- | Render Csound file and save it to the give file.
writeCsd_ :: String -> SE () -> IO ()
writeCsd_ file csd = writeFile file =<< renderCsd_ csd

-- | Render Csound file with options and save it to the give file.
writeCsdBy_ :: Options -> String -> SE () -> IO ()
writeCsdBy_ opt file csd = writeFile file =<< renderCsdBy_ opt csd

-- | Renders csound code to file @tmp.csd@ and plays it with @-odac@ option
-- (sound output goes to soundcard in real time).
dac_ :: SE () -> IO ()
dac_ = dacBy_ def

-- | 'Csound.Base.dac' with options.
dacBy_ :: Options -> SE () -> IO ()
dacBy_ opt csd = do
    writeCsdBy_ opt "tmp.csd" csd
    _ <- system $ "csound -odac " ++ "tmp.csd" 
    return ()


