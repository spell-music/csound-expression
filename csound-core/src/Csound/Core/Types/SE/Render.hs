{-# Language AllowAmbiguousTypes, CPP, UndecidableInstances #-}
module Csound.Core.Types.SE.Render
  (
    -- * Rendering
    RenderCsd(..),
    CsdArity(..),
    renderCsd,
    writeCsd, writeCsdBy,
    writeSnd, writeSndBy,
    printCsd, printCsdBy,

    -- * Playing the sound
    playCsd, playCsdBy,
    mplayer, mplayerBy, totem, totemBy,

    -- * Live performance
    dac, dacBy, vdac, vdacBy, dacDebug,

    -- * Render and run
    csd, csdBy,

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
    -- readMacrosString, readMacrosDouble, readMacrosInt
  ) where

import Csound.Core.State.Options
  (Options (..), setSilent, setDac, setAdc, setDacBy, setAdcBy, setVirtual, logTrace)
import Csound.Core.Types.SE
import Csound.Core.Types.Prim
import Csound.Core.Types.Tuple

import Control.Monad

import Data.Text qualified as Text
import System.Process
import System.Directory
import System.FilePath
import qualified Control.Exception as E
import Data.Default

data CsdArity = CsdArity
  { csdArity'inputs  :: Int
  , csdArity'outputs :: Int
  } deriving (Show, Eq)

class RenderCsd a where
    renderCsdBy :: Options -> a -> IO String
    csdArity :: CsdArity

hasInputs :: forall a . RenderCsd a => Bool
hasInputs = csdArity'inputs (csdArity @a) > 0

hasOutputs :: forall a . RenderCsd a => Bool
hasOutputs = csdArity'outputs (csdArity @a) > 0

renderEff :: (Sigs a, Sigs b) => Options -> (a -> SE b) -> IO String
renderEff opt instr = renderInstr opt (instr =<< readIns)

renderInstr :: (Sigs a) => Options -> SE a -> IO String
renderInstr opt instr = renderSE opt (writeOuts =<< instr)

instance {-# OVERLAPPING #-} RenderCsd (SE ()) where
    renderCsdBy = renderSE
    csdArity = CsdArity 0 0

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} forall a. Sigs a => RenderCsd a where
  renderCsdBy opt a = renderInstr opt (pure a)
  csdArity = CsdArity 0 (tupleArity @a)

instance {-# OVERLAPPABLE #-} forall a. Sigs a => RenderCsd (SE a) where
  renderCsdBy opt a = renderInstr opt a
  csdArity = CsdArity 0 (tupleArity @a)
#endif

instance {-# OVERLAPPABLE #-} forall a b. (Sigs a, Sigs b) => RenderCsd (a -> b) where
    renderCsdBy opt f = renderEff opt (pure . f)
    csdArity = CsdArity (tupleArity @a) (tupleArity @b)

instance {-# OVERLAPPABLE #-} forall a b. (Sigs a, Sigs b) => RenderCsd (a -> SE b) where
    renderCsdBy opt f = renderEff opt f
    csdArity = CsdArity (tupleArity @a) (tupleArity @b)

-- | Renders Csound file.
renderCsd :: RenderCsd a => a -> IO String
renderCsd = renderCsdBy def

getTmpFile :: IO FilePath
getTmpFile = (</> "tmp.csd") <$> getTemporaryDirectory

-- | Render Csound file and save it to the give file.
writeCsd :: RenderCsd a => FilePath -> a -> IO ()
writeCsd file a = writeFile file =<< renderCsd a

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: RenderCsd a => Options -> FilePath -> a -> IO ()
writeCsdBy opt file a = writeFile file =<< renderCsdBy opt a

-- | Render Csound file and print it on the screen
printCsd :: RenderCsd a => a -> IO ()
printCsd a = printCsdBy def a

-- | Render Csound file with options and print it on the screen
printCsdBy :: RenderCsd a => Options -> a -> IO ()
printCsdBy opt a = putStrLn =<< renderCsdBy opt a

-- | Render Csound file and save result sound to the wav-file.
writeSnd :: RenderCsd a => FilePath -> a -> IO ()
writeSnd = writeSndBy def

-- | Render Csound file with options and save result sound to the wav-file.
writeSndBy :: RenderCsd a => Options -> FilePath -> a -> IO ()
writeSndBy opt file a = do
    fileCsd <- getTmpFile
    writeCsdBy opt fileCsd a
    runWithUserInterrupt (postSetup opt) $ unwords ["csound -o", file, fileCsd, logTrace opt]

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

-- | Makes what @dac@ does and saves Csound code to tmp.csd
dacDebug :: RenderCsd a => a -> IO ()
dacDebug a = do
  writeCsd "tmp.csd" a
  dac a

-- | 'Csound.Base.dac' with options.
dacBy :: forall a. (RenderCsd a) => Options -> a -> IO ()
dacBy opt' a = do
    fileCsd <- getTmpFile
    writeCsdBy opt fileCsd a
    runWithUserInterrupt (postSetup opt') $ unwords ["csound", fileCsd, logTrace opt']
    where
      opt = mconcat [opt', withDac, withAdc]

      withDac
        | hasJackConnections opt'       = setDacBy "null"
        | hasOutputs @a = setDac
        | otherwise                     = mempty

      withAdc
        | hasJackConnections opt'      = setAdcBy "null"
        | hasInputs @a = setAdc
        | otherwise                    = mempty

-- | Output to dac with virtual midi keyboard.
vdac :: (RenderCsd a) => a -> IO ()
vdac = dacBy (setVirtual def)

-- | Output to dac with virtual midi keyboard with specified options.
vdacBy :: (RenderCsd a) => Options -> a -> IO ()
vdacBy opt = dacBy (setVirtual opt)

-- | Renders to file @tmp.csd@ in temporary directory and invokes the csound on it.
csd :: (RenderCsd a) => a -> IO ()
csd = csdBy setSilent

-- | Renders to file @tmp.csd@ in temporary directory and invokes the csound on it.
csdBy :: (RenderCsd a) => Options -> a -> IO ()
csdBy options a = do
    fileCsd <- getTmpFile
    writeCsdBy (setSilent `mappend` options) fileCsd a
    runWithUserInterrupt (postSetup options) $ unwords ["csound", fileCsd, logTrace options]

postSetup :: Options -> IO ()
postSetup opt = jackConnect opt

jackConnect :: Options -> IO ()
jackConnect opt
  | Just conns <- csdJackConnect opt = case conns of
                                         [] -> pure ()
                                         _  -> void $ runCommand $ Text.unpack $ jackCmd conns
  | otherwise                        = pure ()
  where
    addSleep = ("sleep 0.1; " `mappend` )

    jackCmd = addSleep . Text.intercalate ";" . fmap jackConn
    jackConn (port1, port2) = Text.unwords ["jack_connect", port1, port2]

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
    E.catch (waitForProcess pid >> pure ()) (onUserInterrupt pid)
    where
        onUserInterrupt :: ProcessHandle -> E.AsyncException -> IO ()
        onUserInterrupt pid x = case x of
            E.UserInterrupt -> do
              putStrLn "User Interrupt"
              terminateProcess pid
            e               -> E.throw e

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


{-
--------------------------------------------------------------------
-- macroses

readMacrosDouble :: Text -> Double -> SE D
readMacrosDouble = readMacrosBy D.readMacrosDouble MacrosInitDouble

readMacrosString :: Text -> Text -> SE Str
readMacrosString = readMacrosBy D.readMacrosString MacrosInitString

readMacrosInt :: Text -> Int -> SE D
readMacrosInt    = readMacrosBy D.readMacrosInt    MacrosInitInt

readMacrosBy :: (Text ->  E) -> (Text -> a -> MacrosInit) -> Text -> a -> GE E
readMacrosBy extract allocator name initValue = do
    onMacrosInits $ initMacros $ allocator name initValue
    return $ extract name
    where onMacrosInits = onHistory macrosInits (\val h -> h { macrosInits = val })
-}

