{-# Language FlexibleInstances, UndecidableInstances #-}
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
    csd, csdBy,

    -- * Render and run with cabbage
    runCabbage, runCabbageBy,

    -- * Aliases for type inference
    -- | Sometimes the type class @RenderCsd@ is too whide for us.
    -- It cn be hard to use in the interpreter without explicit signatures.
    -- There are functions to help the type inference.
    -- ** For processing inputs
    onCard1, onCard2, onCard4, onCard6, onCard8,

    -- * Config with command line arguments
    -- | With the functions we can add global config parameters to the rendered file.
    -- We can supply different parameters with @--omacro@ flag.
    --
    -- An example:
    --
    -- > dac $ osc (sig $ readMacrosDouble "FREQ" 440)
    --
    -- Here we define frequency as a global parameter. It's available by name @"FREQ"@.
    -- If we run the program with no flags it would play the default 440 Hz. But we can change that like this:
    --
    -- > csound tmp.csd --omacro:FREQ=330
    --
    -- We can update the macro-arguments with flag @--omacro:NAME=VALUE@.
    readMacrosString, readMacrosDouble, readMacrosInt
) where

import System.Process
import qualified Control.Exception as E

import Control.Monad
import Data.Monoid
import Data.Default
import Csound.Typed
import Csound.Control.Gui

import Csound.Options(setSilent, setDac, setCabbage)

render :: Sigs a => Options -> SE a -> IO String
render = renderOutBy

render_ :: Options -> SE () -> IO String
render_ = renderOutBy_

class RenderCsd a where
    renderCsdBy :: Options -> a -> IO String

instance {-# OVERLAPPING #-} RenderCsd (SE ()) where
    renderCsdBy = render_

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd a where
    renderCsdBy opt a = render opt (return a)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (SE a) where
    renderCsdBy opt a = render opt a

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> b) where
    renderCsdBy opt f = renderEffBy opt (return . f)

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> SE b) where
    renderCsdBy opt f = renderEffBy opt f

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> Source b) where
    renderCsdBy opt f = renderEffBy opt (fromSource . f)

instance (Sigs a, Sigs b) => RenderCsd (a -> Source (SE b)) where
    renderCsdBy opt f = renderEffBy opt (fromSourceSE . f)

instance {-# OVERLAPPING #-} (Sigs a) => RenderCsd (a -> Source (SE Sig2)) where
    renderCsdBy opt f = renderEffBy opt (fromSourceSE . f)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (Source a) where
    renderCsdBy opt a = renderCsdBy opt (fromSource a)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (Source (SE a)) where
    renderCsdBy opt a = renderCsdBy opt (fromSourceSE a)

instance {-# OVERLAPPING #-} RenderCsd (Source ()) where
    renderCsdBy opt src = renderCsdBy opt $ do
        (ui, _) <- src
        panel ui

instance {-# OVERLAPPING #-} RenderCsd (Source (SE ())) where
    renderCsdBy opt src = renderCsdBy opt (joinSource src)

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

----------------------------------------------------------

-- | Runs the csound files with cabbage engine.
-- It invokes the Cabbage command line utility and setts all default cabbage flags.
runCabbage :: (RenderCsd a) => a -> IO ()
runCabbage = runCabbageBy def

-- | Runs the csound files with cabbage engine with user defined options.
-- It invokes the Cabbage command line utility and setts all default cabbage flags.
runCabbageBy :: (RenderCsd a) => Options -> a -> IO ()
runCabbageBy opt' a = do
    writeCsdBy opt "tmp.csd" a
    runWithUserInterrupt $ "Cabbage " ++ "tmp.csd"
    where opt = opt' <> setCabbage

------------------------------

-- | Alias to process inputs of single input audio-card.
onCard1 :: (Sig -> a) -> (Sig -> a)
onCard1= id

-- | Alias to process inputs of stereo input audio-card.
onCard2 :: (Sig2 -> a) -> (Sig2 -> a)
onCard2= id

-- | Alias to process inputs of audio-card with 4 inputs.
onCard4 :: (Sig4 -> a) -> (Sig4 -> a)
onCard4= id

-- | Alias to process inputs of audio-card with 6 inputs.
onCard6 :: (Sig6 -> a) -> (Sig6 -> a)
onCard6= id

-- | Alias to process inputs of audio-card with 8 inputs.
onCard8 :: (Sig8 -> a) -> (Sig8 -> a)
onCard8= id
