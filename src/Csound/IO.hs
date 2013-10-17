{-# Language FlexibleInstances #-}
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
    RenderCsd(..),
    renderCsd,  
    writeCsd, writeCsdBy, 
    
    -- * Playing the sound
    playCsd, playCsdBy, 
    mplayer, mplayerBy, totem, totemBy,

    -- * Live performance
    dac, dacBy
) where

import System.Cmd(system)
import Data.Default
import Csound.Typed

render :: Sigs a => Options -> SE a -> IO String
render = renderOutBy 

render_ :: Options -> SE () -> IO String
render_ = renderOutBy_ 

class RenderCsd a where
    renderCsdBy :: Options -> a -> IO String

instance RenderCsd (SE ()) where
    renderCsdBy = render_

instance RenderCsd Sig where
    renderCsdBy opt a = render opt (return a)

instance (Sigs a, Sigs b) => RenderCsd (a, b) where
    renderCsdBy opt a = render opt (return a)

instance (Sigs a, Sigs b, Sigs c) => RenderCsd (a, b, c) where
    renderCsdBy opt a = render opt (return a)

instance (Sigs a, Sigs b, Sigs c, Sigs d) => RenderCsd (a, b, c, d) where
    renderCsdBy opt a = render opt (return a)

instance RenderCsd (SE Sig) where
    renderCsdBy opt a = render opt a

instance (Sigs a, Sigs b) => RenderCsd (SE (a, b)) where
    renderCsdBy opt a = render opt a

instance (Sigs a, Sigs b, Sigs c) => RenderCsd (SE (a, b, c)) where
    renderCsdBy opt a = render opt a

instance (Sigs a, Sigs b, Sigs c, Sigs d) => RenderCsd (SE (a, b, c, d)) where
    renderCsdBy opt a = render opt a

-- | Renders Csound file.
renderCsd :: RenderCsd a => a -> IO String
renderCsd = renderCsdBy def

-- | Render Csound file and save it to the give file.
writeCsd :: RenderCsd a => String -> a -> IO ()
writeCsd file csd = writeFile file =<< renderCsd csd

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: RenderCsd a => Options -> String -> a -> IO ()
writeCsdBy opt file csd = writeFile file =<< renderCsdBy opt csd

-- | Renders Csound file, saves it to the given file, renders with csound command and plays it with the given program.
-- 
-- > playCsd program file csd 
--
-- Produces files @file.csd@ (with 'Csound.Render.Mix.renderCsd') and @file.wav@ (with @csound@) and then invokes:
--
-- > program "file.wav"
playCsd :: (RenderCsd a) => (String -> IO ()) -> String -> a -> IO ()
playCsd = playCsdBy def

-- | Works just like 'Csound.Render.Mix.playCsd' but you can supply csound options.
playCsdBy :: (RenderCsd a) => Options -> (String -> IO ()) -> String -> a -> IO ()
playCsdBy opt player file csd = do
    writeCsdBy opt fileCsd csd
    _ <- system $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    player fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

simplePlayCsdBy :: (RenderCsd a) => Options -> String -> String -> a -> IO ()
simplePlayCsdBy opt player = playCsdBy opt phi
    where phi file = do
            _ <- system $ player ++ " " ++ file
            return ()
            

-- | Renders csound code to file @tmp.csd@ and plays it with @-odac@ option
-- (sound output goes to soundcard in real time).
dac :: (RenderCsd a) => a -> IO ()
dac = dacBy def

-- | 'Csound.Base.dac' with options.
dacBy :: (RenderCsd a) => Options -> a -> IO ()
dacBy opt csd = do
    writeCsdBy opt "tmp.csd" csd
    _ <- system $ "csound -odac " ++ "tmp.csd" 
    return ()

--------------------------------------------------------
-- players

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayer :: (RenderCsd a) => a -> IO ()
mplayer = mplayerBy def

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayerBy :: (RenderCsd a) => Options -> a -> IO ()
mplayerBy opt = simplePlayCsdBy opt "mplayer" "tmp"

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totem :: (RenderCsd a) => a -> IO ()
totem = totemBy def

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totemBy :: (RenderCsd a) => Options -> a -> IO ()
totemBy opt = simplePlayCsdBy opt "totem" "tmp"


