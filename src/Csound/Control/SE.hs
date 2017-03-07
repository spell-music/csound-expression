module Csound.Control.SE(
    SE, Ref, writeRef, readRef, modifyRef, mixRef, newRef, sensorsSE, newGlobalRef, globalSensorsSE,
    newCtrlRef, newGlobalCtrlRef, newClearableGlobalRef, newTab, newGlobalTab,
    whileRef
) where

import Csound.Typed.Control
import Csound.Typed.Types.Tuple

