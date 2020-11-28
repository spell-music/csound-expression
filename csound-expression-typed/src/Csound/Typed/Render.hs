module Csound.Typed.Render(
    renderOut, renderOutBy,
    renderEff, renderEffBy,
    renderOut_, renderOutBy_,
    -- * Options
    module Csound.Typed.GlobalState.Options,
    module Csound.Dynamic.Types.Flags,
    saveUserOptions, getUserOptions
) where

import qualified Data.Map as M
import Data.Default
import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Ord
import Data.List(sortBy, groupBy)
import qualified Data.IntMap as IM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import System.Directory
import System.FilePath
import Text.Read (readMaybe)

import Text.PrettyPrint.Leijen(displayS, renderPretty)

import Csound.Dynamic hiding (csdFlags)
import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Elements(NamedInstrs(..))
import Csound.Typed.GlobalState.Options
import Csound.Typed.Control.Instr
import Csound.Typed.Control(getIns)
import Csound.Dynamic.Types.Flags

import Csound.Typed.Gui.Gui(guiStmt, panelIsKeybdSensitive)
import Csound.Typed.Gui.Cabbage.CabbageLang(ppCabbage)


toCsd :: Tuple a => Maybe Int -> Options -> SE a -> GE Csd
toCsd mnchnls_i options sigs = do
    saveMasterInstr (constArity sigs) (masterExp sigs)
    saveMidiMap  -- save midi innstruments
    handleMissingKeyPannel
    renderHistory mnchnls_i (outArity sigs) options

handleMissingKeyPannel :: GE ()
handleMissingKeyPannel = do
    st <- fmap guis $ getHistory
    if (not $ IM.null $ guiKeyEvents st) && (null $ filter panelIsKeybdSensitive $ guiStateRoots st)
        then do
            saveDefKeybdPanel
        else do
            return ()

renderOut_ :: SE () -> IO String
renderOut_ = renderOutBy_ def

renderOutBy_ :: Options -> SE () -> IO String
renderOutBy_ options sigs = do
    finalOptions <- fmap (maybe options (options <> )) getUserOptions
    evalGE finalOptions $ fmap renderCsd $ toCsd Nothing finalOptions (fmap (const unit) sigs)

renderOut :: Sigs a => SE a -> IO String
renderOut = renderOutBy def

renderOutBy :: Sigs a => Options -> SE a -> IO String
renderOutBy options sigs = do
    finalOptions <- fmap (maybe options (options <> )) getUserOptions
    evalGE finalOptions $ fmap renderCsd $ toCsd Nothing finalOptions sigs

renderEff :: (Sigs a, Sigs b) => (a -> SE b) -> IO String
renderEff = renderEffBy def

renderEffBy :: (Sigs a, Sigs b) => Options -> (a -> SE b) -> IO String
renderEffBy options eff = do
    finalOptions <- fmap (maybe options (options <> )) getUserOptions
    evalGE finalOptions $ fmap renderCsd $ toCsd (Just (arityIns $ funArity eff)) finalOptions (eff =<< getIns)

renderHistory :: Maybe Int -> Int -> Options -> GE Csd
renderHistory mnchnls_i nchnls opt = do
    keyEventListener <- getKeyEventListener
    hist1 <- getHistory
    udos <- fmap verbatim $ liftIO $ renderUdoPlugins hist1
    instr0 <- execDepT $ getInstr0 mnchnls_i nchnls opt udos hist1
    terminatorInstrId <- saveInstr =<< terminatorInstr
    expr2 <- getSysExpr terminatorInstrId
    saveAlwaysOnInstr =<< saveInstr (SE expr2)
    expr3 <- guiInstrExp
    saveAlwaysOnInstr =<< saveInstr (SE expr3)
    hist2 <- getHistory
    let namedIntruments = fmap (\(name, body) -> Instr (InstrLabel name) body) $ unNamedInstrs $ namedInstrs hist2
    let orc = Orc instr0 ((namedIntruments ++ ) $ maybeAppend keyEventListener $ fmap (uncurry Instr) $ instrsContent $ instrs hist2)
    hist3 <- getHistory
    let flags   = reactOnMidi hist3 $ csdFlags opt
        sco     = Sco (Just $ pureGetTotalDurForF0 $ totalDur hist3)
                      (renderGens (genMap hist3) (writeGenMap hist3)) $
                      ((fmap alwaysOn $ alwaysOnInstrs hist3) ++ (getNoteEvents $ notes hist3))
    let plugins = getPlugins opt hist3
    return $ Csd flags orc sco plugins
    where
        renderGens gens writeGens = (fmap swap $ M.toList $ idMapContent  gens) ++ writeGens
        maybeAppend ma = maybe id (:) ma
        getNoteEvents = fmap $ \(instrId, evt) -> (instrId, [evt])

        getPlugins opt hist = case cabbageGui hist of
                Nothing -> []
                Just x  -> [(Plugin "Cabbage" (displayS (renderPretty 1 10000 $ ppCabbage x) ""))]

getInstr0 :: Maybe Int -> Int -> Options -> Dep () -> History -> Dep ()
getInstr0 mnchnls_i nchnls opt udos hist = do
    macroses
    defaultScaleUI <- fmap defScaleUI $ lift getOptions
    globalConstants
    midiAssigns
    midiInitCtrls
    initGlobals
    renderBandLimited (genMap hist) (bandLimitedMap hist)
    userInstr0 hist
    chnUpdateUdo
    udos
    sf2
    jackos
    guiStmt defaultScaleUI $ getPanels hist
    where
        globalConstants = do
            setSr       $ defSampleRate opt
            setKsmps    $ defBlockSize opt
            setNchnls   (max 1 nchnls)
            setZeroDbfs 1
            maybe (return ()) setNchnls_i mnchnls_i

        midiAssigns   = mapM_ renderMidiAssign $ midis hist
        midiInitCtrls = mapM_ renderMidiCtrl   $ midiCtrls hist

        initGlobals = fst $ renderGlobals $ globals $ hist

        sf2 = mapM_ (uncurry sfSetList) $ sfGroup $ sfTable hist
        sfGroup = fmap phi . groupBy (\a b -> getName a == getName b) . sortBy (comparing getName)
            where
                getName = sfName . fst
                phi as = (getName $ head as, fmap (\(sf, index) -> (sfBank sf, sfProgram sf, index)) as)

        macroses = forM_ (fmap snd $ M.toList $ macrosInits hist) $ \x -> case x of
            MacrosInitDouble name value -> initMacrosDouble name value
            MacrosInitString name value -> initMacrosString name value
            MacrosInitInt    name value -> initMacrosInt    name value

        jackos = maybe (return ()) (verbatim . renderJacko) $ csdJacko opt


reactOnMidi :: History -> Flags -> Flags
reactOnMidi h flags
    | midiIsActive h && midiDeviceIsEmpty flags = setMidiDevice flags
    | otherwise                                 = flags
    where
        midiIsActive = not . null . midis
        midiDeviceIsEmpty = isNothing . midiDevice . midiRT
        setMidiDevice x = x { midiRT = (midiRT x) { midiDevice = Just "a" } }

getUserOptions :: IO (Maybe Options)
getUserOptions = do
    mHome <- getAt getHomeDirectory
    mCur  <- getAt getCurrentDirectory
    return $ case (mHome, mCur) of
        (_, Just opt) -> Just opt
        (Just opt, Nothing) -> Just opt
        (Nothing, Nothing) -> Nothing
    where
        getAt getPath = do
            fileName <- fmap rcFileAt getPath
            isExist <- doesFileExist fileName
            if isExist
                then do
                    fileContent <- readFile fileName
                    return $ readMaybe fileContent
                else do
                    return Nothing

rcFileAt :: FilePath -> String
rcFileAt dir = dir </> ".csound-expression-rc"

-- | Saves the user options in the current directory.
--
-- If it's saved in the User's home directory it becomes
-- global options.
saveUserOptions :: Options -> IO ()
saveUserOptions opts = do
    fileName <- fmap rcFileAt getCurrentDirectory
    writeFile fileName (show opts)
