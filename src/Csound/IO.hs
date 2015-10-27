{-# Language FlexibleInstances #-}
-- | Rendering of Csound files and playing the music in real time.
--
-- How are we going to get the sound out of Haskell code? 
-- Instruments are ready and we have written all the scores for them. 
-- Now, it's time to use the rendering functions. We can render haskell expressions
-- to Csound code. A rendering function takes a value that represents a sound (it's a tuple of signals)
-- and produces a string with Csound code. It can take a value that represents 
-- the flags for the csound compiler and global settings ('Csound.Options'). 
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
    writeSnd, writeSndBy,
    
    -- * Playing the sound
    playCsd, playCsdBy, 
    mplayer, mplayerBy, totem, totemBy,

    -- * Live performance
    dac, dacBy, vdac, vdacBy,

    -- * Render and run
    csd, csdBy
) where

import System.Process
import qualified Control.Exception as E

import Data.Monoid
import Data.Default
import Csound.Typed
import Csound.Control.Gui

import Csound.Options(setSilent, setDac)

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

instance RenderCsd Sig2 where
    renderCsdBy opt a = render opt (return a)

instance RenderCsd Sig4 where
    renderCsdBy opt a = render opt (return a)

instance RenderCsd Sig6 where
    renderCsdBy opt a = render opt (return a)

instance RenderCsd Sig8 where
    renderCsdBy opt a = render opt (return a)

instance RenderCsd (Sig8, Sig8) where  
    renderCsdBy opt a = render opt (return a)

instance RenderCsd (Sig8, Sig8, Sig8, Sig8) where  
    renderCsdBy opt a = render opt (return a)

instance RenderCsd (SE Sig) where
    renderCsdBy opt a = render opt a

instance RenderCsd (SE Sig2) where
    renderCsdBy opt a = render opt a

instance RenderCsd (SE Sig4) where
    renderCsdBy opt a = render opt a

instance RenderCsd (SE Sig6) where
    renderCsdBy opt a = render opt a

instance RenderCsd (SE Sig8) where
    renderCsdBy opt a = render opt a

instance RenderCsd (SE (Sig8, Sig8)) where  
    renderCsdBy opt a = render opt a

instance RenderCsd (SE (Sig8, Sig8, Sig8, Sig8)) where  
    renderCsdBy opt a = render opt a

instance (Sigs a) => RenderCsd (a -> Sig) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> Sig2) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> Sig4) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> Sig6) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> Sig8) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> (Sig8, Sig8)) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a) => RenderCsd (a -> (Sig8, Sig8, Sig8, Sig8)) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance (Sigs a, Sigs b) => RenderCsd (a -> SE b) where
    renderCsdBy opt f = renderEffBy opt f

instance RenderCsd (Source Sig) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                return asig

instance RenderCsd (Source Sig2) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                return asig

instance RenderCsd (Source Sig4) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                return asig

instance RenderCsd (Source (SE Sig)) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                asig

instance RenderCsd (Source (SE Sig2)) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                asig
instance RenderCsd (Source (SE Sig4)) where
    renderCsdBy opt a = renderCsdBy opt res
        where res = do
                (gui, asig) <- a
                panel gui
                asig

-- | Renders Csound file.
renderCsd :: RenderCsd a => a -> IO String
renderCsd = renderCsdBy def

-- | Render Csound file and save it to the give file.
writeCsd :: RenderCsd a => String -> a -> IO ()
writeCsd file a = writeFile file =<< renderCsd a

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: RenderCsd a => Options -> String -> a -> IO ()
writeCsdBy opt file a = writeFile file =<< renderCsdBy opt a

-- | Render Csound file and save result sound to the wav-file.
writeSnd :: RenderCsd a => String -> a -> IO ()
writeSnd = writeSndBy def

-- | Render Csound file with options and save result sound to the wav-file.
writeSndBy :: RenderCsd a => Options -> String -> a -> IO ()
writeSndBy opt file a = do
    writeCsdBy opt fileCsd a
    runWithUserInterrupt $ "csound -o " ++ file ++ " " ++ fileCsd
    where fileCsd = "tmp.csd"    

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
playCsdBy opt player file a = do
    writeCsdBy opt fileCsd a
    runWithUserInterrupt $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    player fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

simplePlayCsdBy :: (RenderCsd a) => Options -> String -> String -> a -> IO ()
simplePlayCsdBy opt player = playCsdBy opt phi
    where phi file = do
            runWithUserInterrupt $ player ++ " " ++ file

-- | Renders csound code to file @tmp.csd@ with flags set to @-odac@ and @-Ma@
-- (sound output goes to soundcard in real time).
dac :: (RenderCsd a) => a -> IO ()
dac = dacBy def

-- | 'Csound.Base.dac' with options.
dacBy :: (RenderCsd a) => Options -> a -> IO ()
dacBy opt' a = do
    writeCsdBy opt "tmp.csd" a
    runWithUserInterrupt $ "csound " ++ "tmp.csd" 
    where opt = opt' <> setDac

-- | Output to dac with virtual midi keyboard.
vdac :: (RenderCsd a) => a -> IO ()
vdac = dacBy (setVirtual def) 

-- | Output to dac with virtual midi keyboard with specified options.
vdacBy :: (RenderCsd a) => Options -> a -> IO ()
vdacBy opt = dacBy (setVirtual opt) 

setVirtual :: Options -> Options 
setVirtual a = a { csdFlags = (csdFlags a) { rtmidi = Just VirtualMidi, midiRT = m { midiDevice = Just "0" } } }
    where m = midiRT $ csdFlags a

-- | Renders to file @tmp.csd@ and invokes the csound on it.
csd :: (RenderCsd a) => a -> IO ()
csd = csdBy setSilent

-- | Renders to file @tmp.csd@ and invokes the csound on it.
csdBy :: (RenderCsd a) => Options -> a -> IO ()
csdBy options a = do
    writeCsdBy (setSilent <> options) "tmp.csd" a
    runWithUserInterrupt $ "csound tmp.csd" 


----------------------------------------------------------
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

----------------------------------------------------------
-- handle user interrupts

runWithUserInterrupt :: String -> IO ()
runWithUserInterrupt cmd = do
    pid <- runCommand cmd
    E.catch (waitForProcess pid >> return ()) (onUserInterrupt pid)
    where
        onUserInterrupt :: ProcessHandle -> E.AsyncException -> IO ()
        onUserInterrupt pid x = case x of 
            E.UserInterrupt -> terminateProcess pid >> E.throw x
            e               -> E.throw e

