newInstr :: (Args a, Sigs b) => (a -> SE b) -> SE (InstrId a b)
newInstr = undefined

readInstr :: (Sigs b) => InstrId a b -> SE b
readInstr = undefined

noteOnInstr :: (Args a) => InstrId a b -> D -> a -> SE ()
noteOnInstr = undefined

noteOffInstr :: (Args a) => InstrId a b -> D -> SE ()
noteOffInstr = undefined

----------------------------------

main = do
  instrId <- newInstr $ instrDef

  runEvt (listenOsc port "/play" "s") $ \file -> do
    noteOffInstr instrId 0
    noteOnInstr instrId 0 file

  runEvt (listenOsc port "/stop" "i") $ \_ -> do
    noteOffInstr instrId 0

instrDef :: SE Sig2
instrDef file = return $ diskin file 1

newInstr :: (Args a) => (a -> SE b) -> InstrId a b
newInstr = undefined

do
  ref <- newClearableGlobalRef 0
  instrId <- newInstr $ \cps -> mixRef (sine $ sig cps)

  eventOn (fracId 4 1 instrId) 0 1 440

---------------------------------

newInstr :: (a -> SE ()) -> SE (InstrId a)
newInstr = undefined

event :: InstrId a -> D -> D -> a -> SE ()
event = undefined

negateInstrId :: InstrId a -> InstrId a
negateInstrId = undefined

addFracInstrId :: D -> D -> InstrId a -> InstrId a
addFracInstrId = undefined

noteOn :: D -> D -> InstrId a -> a -> SE ()
noteOn maxSize noteId instrId args = event (addFracInstrId maxSize noteId instrId) 0 (-1) args

noteOff :: D -> D -> InstrId a -> SE ()
noteOff maxSize noteId instrId = event (negateInstrId $ addFracInstrId maxSize noteId instrId) 0 0.01 (def :: a)

--------------------------------

newOutInstr :: (a -> SE b) -> SE (InstrId a, b)
newOutInstr f = do
  ref <- newClearableGlobalRef (0 :: b)
  instrId <- newInstr $ \a -> mixRef ref =<< f a
  aout <- readRef ref
  return (instrId, aout)
