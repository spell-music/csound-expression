module Csound.IO (
    renderCsd, renderCsdBy, 
    writeCsd, writeCsdBy, playCsd, playCsdBy, 
    mplayer, mplayerBy, totem, totemBy,
) where


import System.Cmd(system)
import Data.Default

import Csound.Exp(CsdSco)
import Csound.Exp.Mix(Mix)
import Csound.Exp.Options(CsdOptions)
import Csound.Render(render)
import Csound.Exp.Tuple(Out)

-- | Renders Csound file.
renderCsd :: (Out a, CsdSco sco) => sco (Mix a) -> IO String
renderCsd = renderCsdBy def

-- | Renders Csound file with options.
renderCsdBy :: (Out a, CsdSco sco) => CsdOptions -> sco (Mix a) -> IO String
renderCsdBy opt as = render opt as

-- | Render Csound file and save it to the give file.
writeCsd :: (Out a, CsdSco sco) => String -> sco (Mix a) -> IO ()
writeCsd file sco = writeFile file =<< renderCsd sco 

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: (Out a, CsdSco sco) => CsdOptions -> String -> sco (Mix a) -> IO ()
writeCsdBy opt file sco = writeFile file =<< renderCsdBy opt sco

-- | RenderCsound file save it to the given file, render with csound command and play it with the given program.
-- 
-- > playCsd program file sco 
--
-- Produces files @file.csd@ (with 'Csound.Render.Mix.renderCsd') and @file.wav@ (with @csound@) and then invokes:
--
-- > program file.wav
playCsd :: (Out a, CsdSco sco) => String -> String -> sco (Mix a) -> IO ()
playCsd = playCsdBy def

-- | Works just like 'Csound.Render.Mix.playCsd' but you can supply csound options.
playCsdBy :: (Out a, CsdSco sco) => CsdOptions -> String -> String -> sco (Mix a) -> IO ()
playCsdBy opt player file sco = do
    writeCsdBy opt fileCsd sco
    _ <- system $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    _ <- system $ player ++ " " ++ fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

--------------------------------------------------------
-- players

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayer :: (Out a, CsdSco sco) => sco (Mix a) -> IO ()
mplayer = mplayerBy def

-- | Renders to tmp.csd and tmp.wav and plays with mplayer.
mplayerBy :: (Out a, CsdSco sco) => CsdOptions -> sco (Mix a) -> IO ()
mplayerBy opt = playCsdBy opt "mplayer" "tmp"

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totem :: (Out a, CsdSco sco) => sco (Mix a) -> IO ()
totem = totemBy def

-- | Renders to tmp.csd and tmp.wav and plays with totem player.
totemBy :: (Out a, CsdSco sco) => CsdOptions -> sco (Mix a) -> IO ()
totemBy opt = playCsdBy opt "totem" "tmp"

