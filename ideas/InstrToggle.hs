
newInstr :: (Args a, Sigs b) => (a -> SE b)-> SE (InstrId a b)
newInstr = undefined

readInstr :: Sigs b => InstrId a b -> SE b
readInstr = undefined

noteOnInstr :: Args a => InstrId a b -> D -> a -> SE ()
noteOnInstr = undefined

noteOffInstr :: Args a => InstrId a b -> D -> SE ()
noteOffInstr = undefined

----------------------------------

main = do    
    instrId <- newInstr $ instrDef

    runEvt (listenOsc port "/play" "s") $ \file -> do
        noteOffInstr instrId 0
        noteOnInstr  instrId 0 file

    runEvt (listenOsc port "/stop" "i") $ \_ -> do
        noteOffInstr instrId 0      

instrDef :: SE Sig2
instrDef file = return $ diskin file 1



