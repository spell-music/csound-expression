{-# Language FlexibleInstances, UndecidableInstances, CPP #-}
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
    CsdArity(..),
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

    -- * Save user options
    saveUserOptions,

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

--import Control.Concurrent
import Control.Monad

import System.Process
import qualified Control.Exception as E

import Data.Default
import Csound.Typed
import Csound.Control.Gui

import Csound.Options(setSilent, setDac, setAdc, setDacBy, setAdcBy, setCabbage)
import Temporal.Class(Harmony(..))

import qualified Data.List as L

render :: Sigs a => Options -> SE a -> IO String
render = renderOutBy

render_ :: Options -> SE () -> IO String
render_ = renderOutBy_

data CsdArity = CsdArity
  { csdArity'inputs  :: Int
  , csdArity'outputs :: Int
  } deriving (Show, Eq)

class RenderCsd a where
    renderCsdBy :: Options -> a -> IO String
    csdArity :: a -> CsdArity

hasInputs :: RenderCsd a => a -> Bool
hasInputs = ( > 0) . csdArity'inputs . csdArity

hasOutputs :: RenderCsd a => a -> Bool
hasOutputs = ( > 0) . csdArity'outputs . csdArity

instance {-# OVERLAPPING #-} RenderCsd (SE ()) where
    renderCsdBy = render_
    csdArity _ = CsdArity 0 0

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd a where
  renderCsdBy opt a = render opt (return a)
  csdArity a = CsdArity 0 (tupleArity a)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (SE a) where
  renderCsdBy opt a = render opt a
  csdArity a = CsdArity 0 (outArity a)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (Source a) where
  renderCsdBy opt a = renderCsdBy opt (fromSource a)
  csdArity a = CsdArity 0 (tupleArity $ proxySource a)
      where

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (Source (SE a)) where
  renderCsdBy opt a = renderCsdBy opt (fromSourceSE a)
  csdArity a = CsdArity 0 (tupleArity $ proxySE $ proxySource a)

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd (Sco (Mix a)) where
  renderCsdBy opt a = renderCsdBy opt (mix a)
  csdArity a = CsdArity 0 (tupleArity $ proxy a)
    where
      proxy :: Sco (Mix a) -> a
      proxy = const undefined

instance {-# OVERLAPPABLE #-} Sigs a => RenderCsd [Sco (Mix a)] where
  renderCsdBy opt a = renderCsdBy opt (mix $ har a)
  csdArity a = CsdArity 0 (tupleArity $ proxy a)
    where
      proxy :: [Sco (Mix a)] -> a
      proxy = const undefined

#endif

proxySource :: Source a -> a
proxySource = const undefined

proxySE :: SE a -> a
proxySE = const undefined

proxyFun :: (a -> b) -> (a, b)
proxyFun = const undefined

proxyIn :: (a -> b) -> a
proxyIn = fst . proxyFun

proxyOut :: (a -> b) -> b
proxyOut = snd . proxyFun

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> b) where
    renderCsdBy opt f = renderEffBy opt (return . f)
    csdArity a = CsdArity (tupleArity $ proxyIn a) (tupleArity $ proxyOut a)

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> SE b) where
    renderCsdBy opt f = renderEffBy opt f
    csdArity a = CsdArity (tupleArity $ proxyIn a) (tupleArity $ proxySE $ proxyOut a)

instance {-# OVERLAPPABLE #-} (Sigs a, Sigs b) => RenderCsd (a -> Source b) where
    renderCsdBy opt f = renderEffBy opt (fromSource . f)
    csdArity a = CsdArity (tupleArity $ proxyIn a) (tupleArity $ proxySource $ proxyOut a)

instance (Sigs a, Sigs b) => RenderCsd (a -> Source (SE b)) where
    renderCsdBy opt f = renderEffBy opt (fromSourceSE . f)
    csdArity a = CsdArity (tupleArity $ proxyIn a) (tupleArity $ proxySE $ proxySource $ proxyOut a)

instance {-# OVERLAPPING #-} (Sigs a) => RenderCsd (a -> Source (SE Sig2)) where
    renderCsdBy opt f = renderEffBy opt (fromSourceSE . f)
    csdArity a = CsdArity (tupleArity $ proxyIn a) (tupleArity $ proxySE $ proxySource $ proxyOut a)

instance {-# OVERLAPPING #-} RenderCsd (Source ()) where
    renderCsdBy opt src = renderCsdBy opt $ do
        (ui, _) <- src
        panel ui
    csdArity _ = CsdArity 0 0

instance {-# OVERLAPPING #-} RenderCsd (Source (SE ())) where
    renderCsdBy opt src = renderCsdBy opt (joinSource src)
    csdArity _ = CsdArity 0 0

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
    runWithUserInterrupt (postSetup opt) $ unwords ["csound -o", file, fileCsd, logTrace opt]
    where fileCsd = "tmp.csd"

logTrace :: Options -> String
logTrace opt
  | csdNeedTrace opt = ""
  | otherwise        = "--logfile=null"

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
    runWithUserInterrupt (postSetup opt) $ unwords ["csound -o", fileWav, fileCsd, logTrace opt]
    player fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"

simplePlayCsdBy :: (RenderCsd a) => Options -> String -> String -> a -> IO ()
simplePlayCsdBy opt player = playCsdBy opt phi
    where phi file = do
            runWithUserInterrupt (pure ()) $ unwords [player, file]

-- | Renders csound code to file @tmp.csd@ with flags set to @-odac@, @-iadc@ and @-Ma@
-- (sound output goes to soundcard in real time).
dac :: (RenderCsd a) => a -> IO ()
dac = dacBy def

-- | 'Csound.Base.dac' with options.
dacBy :: (RenderCsd a) => Options -> a -> IO ()
dacBy opt' a = do
    writeCsdBy opt "tmp.csd" a
    runWithUserInterrupt (postSetup opt') $ unwords ["csound tmp.csd", logTrace opt']
    where
      opt = opt' <> withDac <> withAdc

      withDac
        | hasJackConnections opt' = setDacBy "null"
        | hasOutputs a            = setDac
        | otherwise               = mempty

      withAdc
        | hasJackConnections opt' = setAdcBy "null"
        | hasInputs a             = setAdc
        | otherwise               = mempty

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
    runWithUserInterrupt (postSetup options) $ unwords ["csound tmp.csd", logTrace options]

postSetup :: Options -> IO ()
postSetup opt = jackConnect opt

jackConnect :: Options -> IO ()
jackConnect opt
  | Just conns <- csdJackConnect opt = case conns of
                                         [] -> pure ()
                                         _  -> void $ runCommand $ jackCmd conns
  | otherwise                        = pure ()
  where
    addSleep = ("sleep 0.1; " <> )

    jackCmd = addSleep . L.intercalate ";" . fmap jackConn
    jackConn (port1, port2) = unwords ["jack_connect", port1, port2]

hasJackConnections :: Options -> Bool
hasJackConnections opt
  | Just conns <- csdJackConnect opt = not $ null conns
  | otherwise                        = False

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

runWithUserInterrupt :: IO () -> String -> IO ()
runWithUserInterrupt setup cmd = do
    pid <- runCommand cmd
    setup
    E.catch (waitForProcess pid >> return ()) (onUserInterrupt pid)
    where
        onUserInterrupt :: ProcessHandle -> E.AsyncException -> IO ()
        onUserInterrupt pid x = case x of
            E.UserInterrupt -> terminateProcess pid
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
    runWithUserInterrupt (pure ()) $ "Cabbage tmp.csd"
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


#if __GLASGOW_HASKELL__ < 710

-- Sig
setArity n a = CsdArity 0 n

instance RenderCsd Sig                  where { renderCsdBy opt a = render opt (return a)             , csdArity = setArity 1 }
instance RenderCsd (SE Sig)             where { renderCsdBy opt a = render opt a }                    , csdArity = setArity 1 }
instance RenderCsd (Source Sig)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)    , csdArity = setArity 1 }
instance RenderCsd (Source (SE Sig))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a)  , csdArity = setArity 1 }

-- Sig2

instance RenderCsd Sig2                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 2 }
instance RenderCsd (SE Sig2)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 2 }
instance RenderCsd (Source Sig2)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 2 }
instance RenderCsd (Source (SE Sig2))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 2 }

-- Sig3

instance RenderCsd Sig3                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 3 }
instance RenderCsd (SE Sig3)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 3 }
instance RenderCsd (Source Sig3)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 3 }
instance RenderCsd (Source (SE Sig3))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 3 }

-- Sig4

instance RenderCsd Sig4                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 4 }
instance RenderCsd (SE Sig4)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 4 }
instance RenderCsd (Source Sig4)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 4 }
instance RenderCsd (Source (SE Sig4))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 4 }

-- Sig5

instance RenderCsd Sig5                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 5 }
instance RenderCsd (SE Sig5)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 5 }
instance RenderCsd (Source Sig5)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 5 }
instance RenderCsd (Source (SE Sig5))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 5 }

-- Sig6

instance RenderCsd Sig6                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 6 }
instance RenderCsd (SE Sig6)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 6 }
instance RenderCsd (Source Sig6)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 6 }
instance RenderCsd (Source (SE Sig6))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 6 }

-- Sig7

instance RenderCsd Sig7                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 7 }
instance RenderCsd (SE Sig7)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 7 }
instance RenderCsd (Source Sig7)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 7 }
instance RenderCsd (Source (SE Sig7))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 7 }


-- Sig8

instance RenderCsd Sig8                  where { renderCsdBy opt a = render opt (return a)            , csdArity = setArity 8 }
instance RenderCsd (SE Sig8)             where { renderCsdBy opt a = render opt a                     , csdArity = setArity 8 }
instance RenderCsd (Source Sig8)         where { renderCsdBy opt a = renderCsdBy opt (fromSource a)   , csdArity = setArity 8 }
instance RenderCsd (Source (SE Sig8))    where { renderCsdBy opt a = renderCsdBy opt (fromSourceSE a) , csdArity = setArity 8 }


instance RenderCsd (Sco (Mix Sig))       where { renderCsdBy opt a = renderCsdBy opt $ mix a , csdArity = setArity 1 }
instance RenderCsd (Sco (Mix Sig2))      where { renderCsdBy opt a = renderCsdBy opt $ mix a , csdArity = setArity 2 }
instance RenderCsd (Sco (Mix Sig3))      where { renderCsdBy opt a = renderCsdBy opt $ mix a , csdArity = setArity 3 }
instance RenderCsd (Sco (Mix Sig4))      where { renderCsdBy opt a = renderCsdBy opt $ mix a , csdArity = setArity 4 }
instance RenderCsd (Sco (Mix Sig5))      where { renderCsdBy opt a = renderCsdBy opt $ mix a , csdArity = setArity 5 }

instance RenderCsd [Sco (Mix Sig)]       where { renderCsdBy opt a = renderCsdBy opt $ mix $ har a , csdArity = setArity 1 }
instance RenderCsd [Sco (Mix Sig2)]      where { renderCsdBy opt a = renderCsdBy opt $ mix $ har a , csdArity = setArity 2 }
instance RenderCsd [Sco (Mix Sig3)]      where { renderCsdBy opt a = renderCsdBy opt $ mix $ har a , csdArity = setArity 3 }
instance RenderCsd [Sco (Mix Sig4)]      where { renderCsdBy opt a = renderCsdBy opt $ mix $ har a , csdArity = setArity 4 }
instance RenderCsd [Sco (Mix Sig5)]      where { renderCsdBy opt a = renderCsdBy opt $ mix $ har a , csdArity = setArity 5 }

#endif
