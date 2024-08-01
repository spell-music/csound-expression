module Csound.Typed.Opcode.FLTK (
  -- * Containers.
  flGroup,
  flGroupEnd,
  flPack,
  flPackEnd,
  flPanel,
  flPanelEnd,
  flScroll,
  flScrollEnd,
  flTabs,
  flTabsEnd,

  -- * Valuators.
  flCount,
  flJoy,
  flKnob,
  flRoller,
  flSlider,
  flText,

  -- * Other.
  flBox,
  flButBank,
  flButton,
  flCloseButton,
  flExecButton,
  flGetsnap,
  flHvsBox,
  flHvsBoxSetValue,
  flKeyIn,
  flLoadsnap,
  flMouse,
  flPrintk,
  flPrintk2,
  flRun,
  flSavesnap,
  flSetsnap,
  flSetSnapGroup,
  flSetVal,
  flSetVal_i,
  flSlidBnk,
  flSlidBnk2,
  flSlidBnk2Set,
  flSlidBnk2Setk,
  flSlidBnkGetHandle,
  flSlidBnkSet,
  flSlidBnkSetk,
  flUpdate,
  flValue,
  flVkeybd,
  flVslidBnk,
  flVslidBnk2,
  flXyin,
  vphaseseg,

  -- * Appearance.
  flColor,
  flColor2,
  flHide,
  flLabel,
  flSetAlign,
  flSetBox,
  flSetColor,
  flSetColor2,
  flSetFont,
  flSetPosition,
  flSetSize,
  flSetText,
  flSetTextColor,
  flSetTextSize,
  flSetTextType,
  flShow,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic
import Csound.Typed
import Data.Proxy

-- Containers.

{- |
A FLTK container opcode that groups child widgets.

>  FLgroup  "label", iwidth, iheight, ix, iy [, iborder] [, image]

csound doc: <https://csound.com/docs/manual/FLgroup.html>
-}
flGroup :: Str -> D -> D -> D -> D -> SE ()
flGroup b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "FLgroup" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Marks the end of a group of FLTK child widgets.

>  FLgroupEnd

csound doc: <https://csound.com/docs/manual/FLgroupEnd.html>
-}
flGroupEnd :: SE ()
flGroupEnd =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLgroupEnd" [(Xr, [])] []

{- |
Provides the functionality of compressing and aligning FLTK widgets.

FLpack provides the functionality of compressing and aligning widgets.

>  FLpack  iwidth, iheight, ix, iy, itype, ispace, iborder

csound doc: <https://csound.com/docs/manual/FLpack.html>
-}
flPack :: D -> D -> D -> D -> D -> D -> D -> SE ()
flPack b1 b2 b3 b4 b5 b6 b7 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7
  where
    f a1 a2 a3 a4 a5 a6 a7 = opcsDep_ "FLpack" [(Xr, [Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6, a7]

{- |
Marks the end of a group of compressed or aligned FLTK widgets.

>  FLpackEnd

csound doc: <https://csound.com/docs/manual/FLpackEnd.html>
-}
flPackEnd :: SE ()
flPackEnd =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLpackEnd" [(Xr, [])] []

{- |
Creates a window that contains FLTK widgets.

>  FLpanel  "label", iwidth, iheight [, ix] [, iy] [, iborder] [, ikbdcapture] [, iclose]

csound doc: <https://csound.com/docs/manual/FLpanel.html>
-}
flPanel :: Str -> D -> D -> SE ()
flPanel b1 b2 b3 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLpanel" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Marks the end of a group of FLTK widgets contained inside of a window (panel).

>  FLpanelEnd

csound doc: <https://csound.com/docs/manual/FLpanelEnd.html>
-}
flPanelEnd :: SE ()
flPanelEnd =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLpanelEnd" [(Xr, [])] []

{- |
A FLTK opcode that adds scroll bars to an area.

FLscroll adds scroll bars to an area.

>  FLscroll  iwidth, iheight [, ix] [, iy]

csound doc: <https://csound.com/docs/manual/FLscroll.html>
-}
flScroll :: D -> D -> SE ()
flScroll b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLscroll" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2]

{- |
A FLTK opcode that marks the end of an area with scrollbars.

>  FLscrollEnd

csound doc: <https://csound.com/docs/manual/FLscrollEnd.html>
-}
flScrollEnd :: SE ()
flScrollEnd =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLscrollEnd" [(Xr, [])] []

{- |
Creates a tabbed FLTK interface.

FLtabs is a âfile card tabsâ interface that is useful to display several areas containing widgets in the same windows, alternatively. It must be used together with FLgroup, another container that groups child widgets.

>  FLtabs  iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLtabs.html>
-}
flTabs :: D -> D -> D -> D -> SE ()
flTabs b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLtabs" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Marks the end of a tabbed FLTK interface.

>  FLtabsEnd

csound doc: <https://csound.com/docs/manual/FLtabsEnd.html>
-}
flTabsEnd :: SE ()
flTabsEnd =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLtabsEnd" [(Xr, [])] []

-- Valuators.

{- |
A FLTK widget opcode that creates a counter.

Allows the user to increase/decrease a value with mouse clicks on a corresponding arrow button.

> kout, ihandle  FLcount  "label", imin, imax, istep1, istep2, itype, \
>           iwidth, iheight, ix, iy, iopcode [, kp1] [, kp2] [, kp3] [...] [, kpN]

csound doc: <https://csound.com/docs/manual/FLcount.html>
-}
flCount :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flCount b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9 <*> (lift . unD) b10 <*> (lift . unD) b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      mopcsDep
        2
        "FLcount"
        ( [Kr, Ir]
        , [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir] ++ (repeat Kr)
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
A FLTK opcode that acts like a joystick.

FLjoy is a squared area that allows the user to modify two output values at the same time. It acts like a joystick.

> koutx, kouty, ihandlex, ihandley  FLjoy  "label", iminx, imaxx, iminy, \
>           imaxy, iexpx, iexpy, idispx, idispy, iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLjoy.html>
-}
flJoy :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, Sig, D, D)
flJoy b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9 <*> (lift . unD) b10 <*> (lift . unD) b11 <*> (lift . unD) b12 <*> (lift . unD) b13
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
      mopcsDep
        4
        "FLjoy"
        ( [Kr, Kr, Ir, Ir]
        , [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13]

{- |
A FLTK widget opcode that creates a knob.

> kout, ihandle  FLknob  "label", imin, imax, iexp, itype, idisp, iwidth, \
>           ix, iy [, icursorsize]

csound doc: <https://csound.com/docs/manual/FLknob.html>
-}
flKnob :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flKnob b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      mopcsDep
        2
        "FLknob"
        ([Kr, Ir], [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

{- |
A FLTK widget that creates a transversal knob.

FLroller is a sort of knob, but put transversally.

> kout, ihandle  FLroller  "label", imin, imax, istep, iexp, itype, idisp, \
>           iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLroller.html>
-}
flRoller :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flRoller b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9 <*> (lift . unD) b10 <*> (lift . unD) b11
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
      mopcsDep
        2
        "FLroller"
        ( [Kr, Ir]
        , [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

{- |
Puts a slider into the corresponding FLTK container.

FLslider puts a slider into the corresponding container.

> kout, ihandle  FLslider  "label", imin, imax, iexp, itype, idisp, iwidth, \
>           iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLslider.html>
-}
flSlider :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flSlider b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9 <*> (lift . unD) b10
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
      mopcsDep
        2
        "FLslider"
        ( [Kr, Ir]
        , [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

{- |
A FLTK widget opcode that creates a textbox.

FLtext allows the user to modify a parameter value by directly typing it into a text field.

> kout, ihandle  FLtext  "label", imin, imax, istep, itype, iwidth, \
>           iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLtext.html>
-}
flText :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flText b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      mopcsDep
        2
        "FLtext"
        ([Kr, Ir], [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        [ a1
        , a2
        , a3
        , a4
        , a5
        , a6
        , a7
        , a8
        , a9
        ]

-- Other.

{- |
A FLTK widget that displays text inside of a box.

> ihandle  FLbox  "label", itype, ifont, isize, iwidth, iheight, ix, iy [, image]
> ihandle  FLbox  istr, itype, ifont, isize, iwidth, iheight, ix, iy [, image]

csound doc: <https://csound.com/docs/manual/FLbox.html>
-}
flBox :: Str -> D -> D -> D -> D -> D -> D -> D -> SE D
flBox b1 b2 b3 b4 b5 b6 b7 b8 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      opcsDep
        "FLbox"
        [ (Ir, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        , (Ir, [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])
        ]
        [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
A FLTK widget opcode that creates a bank of buttons.

> kout, ihandle  FLbutBank  itype, inumx, inumy, iwidth, iheight, ix, iy, \
>           iopcode [, kp1] [, kp2] [, kp3] [, kp4] [, kp5] [....] [, kpN]

csound doc: <https://csound.com/docs/manual/FLbutBank.html>
-}
flButBank :: D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flButBank b1 b2 b3 b4 b5 b6 b7 b8 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      mopcsDep
        2
        "FLbutBank"
        ( [Kr, Ir]
        , [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir] ++ (repeat Kr)
        )
        [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
A FLTK widget opcode that creates a button.

> kout, ihandle  FLbutton  "label", ion, ioff, itype, iwidth, iheight, ix, \
>           iy, iopcode [, kp1] [, kp2] [, kp3] [, kp4] [, kp5] [....] [, kpN]

csound doc: <https://csound.com/docs/manual/FLbutton.html>
-}
flButton :: Str -> D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, D)
flButton b1 b2 b3 b4 b5 b6 b7 b8 b9 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8 <*> (lift . unD) b9
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      mopcsDep
        2
        "FLbutton"
        ( [Kr, Ir]
        , [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir] ++ (repeat Kr)
        )
        [a1, a2, a3, a4, a5, a6, a7, a8, a9]

{- |
A FLTK widget opcode that creates a button that will close the panel
      window it is a part of.

> ihandle  FLcloseButton  "label", iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLcloseButton.html>
-}
flCloseButton :: Str -> D -> D -> D -> D -> SE D
flCloseButton b1 b2 b3 b4 b5 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep "FLcloseButton" [(Ir, [Sr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
A FLTK widget opcode that creates a button that executes a command.

A FLTK widget opcode that creates a button that executes a command. Useful
      for opening up HTML documentation as About text or to start a separate
      program from an FLTK widget interface.

> ihandle  FLexecButton  "command", iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLexecButton.html>
-}
flExecButton :: Str -> D -> D -> D -> D -> SE D
flExecButton b1 b2 b3 b4 b5 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep "FLexecButton" [(Ir, [Sr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
Retrieves a previously stored FLTK snapshot.

Retrieves a previously stored snapshot (in memory), i.e. sets all valuator to the corresponding values stored in that snaphot.

> inumsnap  FLgetsnap  index [, igroup]

csound doc: <https://csound.com/docs/manual/FLgetsnap.html>
-}
flGetsnap :: D -> SE D
flGetsnap b1 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep "FLgetsnap" [(Ir, [Ir, Ir])] [a1]

{- |
Displays a box with a grid useful for visualizing two-dimensional Hyper Vectorial Synthesis.

FLhvsBox displays a box with a grid useful for visualizing two-dimensional Hyper Vectorial Synthesis.

> ihandle  FLhvsBox  inumlinesX, inumlinesY, iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLhvsBox.html>
-}
flHvsBox :: D -> D -> D -> D -> D -> D -> SE D
flHvsBox b1 b2 b3 b4 b5 b6 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep "FLhvsBox" [(Ir, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Sets the cursor position of a previously-declared FLhvsBox widget.

FLhvsBoxSetValue sets the cursor position of a previously-declared FLhvsBox widget.

>  FLhvsBoxSetValue  kx, ky, ihandle

csound doc: <https://csound.com/docs/manual/FLhvsBoxSetValue.html>
-}
flHvsBoxSetValue :: Sig -> Sig -> D -> SE ()
flHvsBoxSetValue b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLhvsBoxSetValue" [(Xr, [Kr, Kr, Ir])] [a1, a2, a3]

{- |
Reports keys pressed (on alphanumeric keyboard) when an FLTK panel has focus.

FLkeyIn informs about the status of a key pressed by the user on the alphanumeric keyboard when an FLTK panel has got the focus.

> kascii  FLkeyIn  [ifn]

csound doc: <https://csound.com/docs/manual/FLkeyIn.html>
-}
flKeyIn :: SE Sig
flKeyIn =
  fmap (Sig . return) $ SE $ join $ return $ f
  where
    f = opcsDep "FLkeyIn" [(Kr, [Ir])] []

{- |
Loads all snapshots into the memory bank of the current orchestra.

FLloadsnap loads all the snapshots contained in a file into the memory bank of the current orchestra.

>  FLloadsnap  "filename" [, igroup]

csound doc: <https://csound.com/docs/manual/FLloadsnap.html>
-}
flLoadsnap :: Str -> SE ()
flLoadsnap b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "FLloadsnap" [(Xr, [Sr, Ir])] [a1]

{- |
Returns the mouse position and the state of the three mouse buttons.

FLmouse returns the coordinates of the mouse position within an FLTK panel and the state of the three mouse buttons.

> kx, ky, kb1, kb2, kb3  FLmouse  [imode]

csound doc: <https://csound.com/docs/manual/FLmouse.html>
-}
flMouse :: forall a. (Tuple a) => SE a
flMouse =
  fmap (toTuple . pure) $ SE $ join $ return $ f
  where
    f = mopcsDep (tupleArity (Proxy :: Proxy a)) "FLmouse" ([Kr, Kr, Kr, Kr, Kr], [Ir]) []

{- |
A FLTK opcode that prints a k-rate value at specified intervals.

FLprintk is similar to printk but shows values of a k-rate signal in a text field instead of on the console.

>  FLprintk  itime, kval, idisp

csound doc: <https://csound.com/docs/manual/FLprintk.html>
-}
flPrintk :: D -> Sig -> D -> SE ()
flPrintk b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLprintk" [(Xr, [Ir, Kr, Ir])] [a1, a2, a3]

{- |
A FLTK opcode that prints a new value every time a control-rate variable changes.

FLprintk2 is similar to FLprintk but shows a k-rate variable's value only when it changes.

>  FLprintk2  kval, idisp

csound doc: <https://csound.com/docs/manual/FLprintk2.html>
-}
flPrintk2 :: Sig -> D -> SE ()
flPrintk2 b1 b2 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLprintk2" [(Xr, [Kr, Ir])] [a1, a2]

{- |
Starts the FLTK widget thread.

>  FLrun

csound doc: <https://csound.com/docs/manual/FLrun.html>
-}
flRun :: SE ()
flRun =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLrun" [(Xr, [])] []

{- |
Saves all snapshots currently created into a file.

FLsavesnap saves all snapshots currently created (i.e. the entire memory bank) into a file.

>  FLsavesnap  "filename" [, igroup]

csound doc: <https://csound.com/docs/manual/FLsavesnap.html>
-}
flSavesnap :: Str -> SE ()
flSavesnap b1 =
  SE $ join $ f <$> (lift . unStr) b1
  where
    f a1 = opcsDep_ "FLsavesnap" [(Xr, [Sr, Ir])] [a1]

{- |
Stores the current status of all FLTK valuators into a snapshot location.

FLsetsnap stores the current status of all valuators present in the orchestra into a snapshot location (in memory).

> inumsnap, inumval  FLsetsnap  index [, ifn, igroup]

csound doc: <https://csound.com/docs/manual/FLsetsnap.html>
-}
flSetsnap :: D -> SE (D, D)
flSetsnap b1 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = mopcsDep 2 "FLsetsnap" ([Ir, Ir], [Ir, Ir, Ir]) [a1]

{- |
Determines the snapshot group for FL valuators.

FLsetSnapGroup determines the snapshot group of valuators declared after it.

>  FLsetSnapGroup  igroup

csound doc: <https://csound.com/docs/manual/FLsetSnapGroup.html>
-}
flSetSnapGroup :: D -> SE ()
flSetSnapGroup b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "FLsetSnapGroup" [(Xr, [Ir])] [a1]

{- |
Sets the value of a FLTK valuator at control-rate.

FLsetVal is almost identical to FLsetVal_i. Except it operates at k-rate and it affects the target valuator only when ktrig is set to a non-zero value.

>  FLsetVal  ktrig, kvalue, ihandle

csound doc: <https://csound.com/docs/manual/FLsetVal.html>
-}
flSetVal :: Sig -> Sig -> D -> SE ()
flSetVal b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unSig) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLsetVal" [(Xr, [Kr, Kr, Ir])] [a1, a2, a3]

{- |
Sets the value of a FLTK valuator to a number provided by the user.

FLsetVal_i forces the value of a valuator to a number provided by the user.

>  FLsetVal_i  ivalue, ihandle

csound doc: <https://csound.com/docs/manual/FLsetVal_i.html>
-}
flSetVal_i :: D -> D -> SE ()
flSetVal_i b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetVal_i" [(Xr, [Ir, Ir])] [a1, a2]

{- |
A FLTK widget containing a bank of horizontal sliders.

FLslidBnk is a widget containing a bank of horizontal sliders.

>  FLslidBnk  "names", inumsliders [, ioutable] [, iwidth] [, iheight] [, ix] \
>           [, iy] [, itypetable] [, iexptable] [, istart_index] [, iminmaxtable]

csound doc: <https://csound.com/docs/manual/FLslidBnk.html>
-}
flSlidBnk :: Str -> D -> SE ()
flSlidBnk b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLslidBnk" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
A FLTK widget containing a bank of horizontal sliders.

FLslidBnk2 is a widget containing a bank of horizontal sliders.

>  FLslidBnk2  "names", inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]
>  FLslidBnk2  istring, inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]

csound doc: <https://csound.com/docs/manual/FLslidBnk2.html>
-}
flSlidBnk2 :: Str -> D -> D -> D -> SE ()
flSlidBnk2 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLslidBnk2" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
modify the values of a slider bank.

FLslidBnk2Set modifies the values of a slider bank according to an array of values stored in a table.

>  FLslidBnk2Set  ihandle, ifn [, istartIndex, istartSlid, inumSlid]

csound doc: <https://csound.com/docs/manual/FLslidBnk2Set.html>
-}
flSlidBnk2Set :: D -> Tab -> SE ()
flSlidBnk2Set b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unTab) b2
  where
    f a1 a2 = opcsDep_ "FLslidBnk2Set" [(Xr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
modify the values of a slider bank.

FLslidBnk2Setk modifies the values of a slider bank according to an array of values stored in a table.

>  FLslidBnk2Setk   ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]

csound doc: <https://csound.com/docs/manual/FLslidBnk2Setk.html>
-}
flSlidBnk2Setk :: Sig -> D -> Tab -> SE ()
flSlidBnk2Setk b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "FLslidBnk2Setk" [(Xr, [Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
gets the handle of last slider bank created.

FLslidBnkGetHandle gets the handle of last slider bank created.

> ihandle  FLslidBnkGetHandle

csound doc: <https://csound.com/docs/manual/FLslidBnkGetHandle.html>
-}
flSlidBnkGetHandle :: SE D
flSlidBnkGetHandle =
  fmap (D . return) $ SE $ join $ return $ f
  where
    f = opcsDep "FLslidBnkGetHandle" [(Ir, [])] []

{- |
modify the values of a slider bank.

FLslidBnkSet modifies the values of a slider bank according to an array of values stored in a table.

>  FLslidBnkSet  ihandle, ifn [, istartIndex, istartSlid, inumSlid]

csound doc: <https://csound.com/docs/manual/FLslidBnkSet.html>
-}
flSlidBnkSet :: D -> Tab -> SE ()
flSlidBnkSet b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unTab) b2
  where
    f a1 a2 = opcsDep_ "FLslidBnkSet" [(Xr, [Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
modify the values of a slider bank.

FLslidBnkSetk modifies the values of a slider bank according to an array of values stored in a table.

>  FLslidBnkSetk   ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]

csound doc: <https://csound.com/docs/manual/FLslidBnkSetk.html>
-}
flSlidBnkSetk :: Sig -> D -> Tab -> SE ()
flSlidBnkSetk b1 b2 b3 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unTab) b3
  where
    f a1 a2 a3 = opcsDep_ "FLslidBnkSetk" [(Xr, [Kr, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
Same as the FLrun opcode.

>  FLupdate

csound doc: <https://csound.com/docs/manual/FLupdate.html>
-}
flUpdate :: SE ()
flUpdate =
  SE $ join $ return $ f
  where
    f = opcsDep_ "FLupdate" [(Xr, [])] []

{- |
Shows the current value of a FLTK valuator.

FLvalue shows current the value of a valuator in a text field.

> ihandle  FLvalue  "label", iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLvalue.html>
-}
flValue :: Str -> D -> D -> D -> D -> SE D
flValue b1 b2 b3 b4 b5 =
  fmap (D . return) $ SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep "FLvalue" [(Ir, [Sr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
An FLTK widget opcode that creates a virtual keyboard widget.

An FLTK widget opcode that creates a virtual keyboard widget. This must
      be used in conjunction with the virtual midi keyboard driver for this to
      operate correctly.  The purpose of this opcode is for making demo versions
      of MIDI orchestras with the virtual keyboard embedded within the main
      window.

>  FLvkeybd  "keyboard.map", iwidth, iheight, ix, iy

csound doc: <https://csound.com/docs/manual/FLvkeybd.html>
-}
flVkeybd :: Str -> D -> D -> D -> D -> SE ()
flVkeybd b1 b2 b3 b4 b5 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5
  where
    f a1 a2 a3 a4 a5 = opcsDep_ "FLvkeybd" [(Xr, [Sr, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5]

{- |
A FLTK widget containing a bank of vertical sliders.

FLvslidBnk is a widget containing a bank of vertical sliders.

>  FLvslidBnk  "names", inumsliders [, ioutable] [, iwidth] [, iheight] [, ix] \
>           [, iy] [, itypetable] [, iexptable] [, istart_index] [, iminmaxtable]

csound doc: <https://csound.com/docs/manual/FLvslidBnk.html>
-}
flVslidBnk :: Str -> D -> SE ()
flVslidBnk b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLvslidBnk" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2]

{- |
A FLTK widget containing a bank of vertical sliders.

FLvslidBnk2 is a widget containing a bank of vertical sliders.

>  FLvslidBnk2  "names", inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]

csound doc: <https://csound.com/docs/manual/FLvslidBnk2.html>
-}
flVslidBnk2 :: Str -> D -> D -> D -> SE ()
flVslidBnk2 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLvslidBnk2" [(Xr, [Sr, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Senses the mouse cursor position in a user-defined area inside an FLpanel.

Similar to xyin, sense the mouse cursor position in a user-defined area inside an FLpanel.

> koutx, kouty, kinside   FLxyin  ioutx_min, ioutx_max, iouty_min, iouty_max, \
>           iwindx_min, iwindx_max, iwindy_min, iwindy_max [, iexpx, iexpy, ioutx, iouty]

csound doc: <https://csound.com/docs/manual/FLxyin.html>
-}
flXyin :: D -> D -> D -> D -> D -> D -> D -> D -> SE (Sig, Sig, Sig)
flXyin b1 b2 b3 b4 b5 b6 b7 b8 =
  fmap (toTuple . pure) $ SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6 <*> (lift . unD) b7 <*> (lift . unD) b8
  where
    f a1 a2 a3 a4 a5 a6 a7 a8 =
      mopcsDep
        3
        "FLxyin"
        ( [Kr, Kr, Kr]
        , [Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir, Ir]
        )
        [a1, a2, a3, a4, a5, a6, a7, a8]

{- |
Allows one-dimensional HVS (Hyper-Vectorial Synthesis).

vphaseseg allows one-dimensional HVS (Hyper-Vectorial Synthesis).

>  vphaseseg  kphase, ioutab, ielems, itab1,idist1,itab2 \
>           [,idist2,itab3, ... ,idistN-1,itabN]

csound doc: <https://csound.com/docs/manual/vphaseseg.html>
-}
vphaseseg :: Sig -> D -> D -> [D] -> SE ()
vphaseseg b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unSig) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> mapM (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "vphaseseg" [(Xr, [Kr] ++ (repeat Ir))] ([a1, a2, a3] ++ a4)

-- Appearance.

{- |
A FLTK opcode that sets the primary colors.

Sets the primary colors to RGB values given by the user.

>  FLcolor  ired, igreen, iblue [, ired2, igreen2, iblue2]

csound doc: <https://csound.com/docs/manual/FLcolor.html>
-}
flColor :: D -> D -> D -> SE ()
flColor b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLcolor" [(Xr, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3]

{- |
A FLTK opcode that sets the secondary (selection) color.

FLcolor2 is the same of FLcolor except it affects the secondary (selection) color.

>  FLcolor2  ired, igreen, iblue

csound doc: <https://csound.com/docs/manual/FLcolor2.html>
-}
flColor2 :: D -> D -> D -> SE ()
flColor2 b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLcolor2" [(Xr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Hides the target FLTK widget.

Hides the target FLTK widget, making it invisible.

>  FLhide  ihandle

csound doc: <https://csound.com/docs/manual/FLhide.html>
-}
flHide :: D -> SE ()
flHide b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "FLhide" [(Xr, [Ir])] [a1]

{- |
A FLTK opcode that modifies the appearance of a text label.

Modifies a set of parameters related to the text label appearence of a widget (i.e. size, font, alignment and color of corresponding text).

>  FLlabel  isize, ifont, ialign, ired, igreen, iblue

csound doc: <https://csound.com/docs/manual/FLlabel.html>
-}
flLabel :: D -> D -> D -> D -> D -> D -> SE ()
flLabel b1 b2 b3 b4 b5 b6 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4 <*> (lift . unD) b5 <*> (lift . unD) b6
  where
    f a1 a2 a3 a4 a5 a6 = opcsDep_ "FLlabel" [(Xr, [Ir, Ir, Ir, Ir, Ir, Ir])] [a1, a2, a3, a4, a5, a6]

{- |
Sets the text alignment of a label of a FLTK widget.

FLsetAlign sets the text alignment of the label of the target widget.

>  FLsetAlign  ialign, ihandle

csound doc: <https://csound.com/docs/manual/FLsetAlign.html>
-}
flSetAlign :: D -> D -> SE ()
flSetAlign b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetAlign" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Sets the appearance of a box surrounding a FLTK widget.

FLsetBox sets the appearance of a box surrounding the target widget.

>  FLsetBox  itype, ihandle

csound doc: <https://csound.com/docs/manual/FLsetBox.html>
-}
flSetBox :: D -> D -> SE ()
flSetBox b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetBox" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Sets the primary color of a FLTK widget.

FLsetColor sets the primary color of the target widget.

>  FLsetColor  ired, igreen, iblue, ihandle

csound doc: <https://csound.com/docs/manual/FLsetColor.html>
-}
flSetColor :: D -> D -> D -> D -> SE ()
flSetColor b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLsetColor" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Sets the secondary (or selection) color of a FLTK widget.

FLsetColor2 sets the secondary (or selection) color of the target widget.

>  FLsetColor2  ired, igreen, iblue, ihandle

csound doc: <https://csound.com/docs/manual/FLsetColor2.html>
-}
flSetColor2 :: D -> D -> D -> D -> SE ()
flSetColor2 b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLsetColor2" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Sets the font type of a FLTK widget.

FLsetFont sets the font type of the target widget.

>  FLsetFont  ifont, ihandle

csound doc: <https://csound.com/docs/manual/FLsetFont.html>
-}
flSetFont :: D -> D -> SE ()
flSetFont b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetFont" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Sets the position of a FLTK widget.

FLsetPosition sets the position of the target widget according to the ix and iy arguments.

>  FLsetPosition  ix, iy, ihandle

csound doc: <https://csound.com/docs/manual/FLsetPosition.html>
-}
flSetPosition :: D -> D -> D -> SE ()
flSetPosition b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLsetPosition" [(Xr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Resizes a FLTK widget.

FLsetSize resizes the target widget (not the size of its text) according to the iwidth and iheight arguments.

>  FLsetSize  iwidth, iheight, ihandle

csound doc: <https://csound.com/docs/manual/FLsetSize.html>
-}
flSetSize :: D -> D -> D -> SE ()
flSetSize b1 b2 b3 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3
  where
    f a1 a2 a3 = opcsDep_ "FLsetSize" [(Xr, [Ir, Ir, Ir])] [a1, a2, a3]

{- |
Sets the label of a FLTK widget.

FLsetText sets the label of the target widget to the double-quoted text string provided with the itext argument.

>  FLsetText  "itext", ihandle
>  FLsetText  istr, ihandle

csound doc: <https://csound.com/docs/manual/FLsetText.html>
-}
flSetText :: Str -> D -> SE ()
flSetText b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetText" [(Xr, [Sr, Ir])] [a1, a2]

{- |
Sets the color of the text label of a FLTK widget.

FLsetTextColor sets the color of the text label of the target widget.

>  FLsetTextColor  ired, iblue, igreen, ihandle

csound doc: <https://csound.com/docs/manual/FLsetTextColor.html>
-}
flSetTextColor :: D -> D -> D -> D -> SE ()
flSetTextColor b1 b2 b3 b4 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2 <*> (lift . unD) b3 <*> (lift . unD) b4
  where
    f a1 a2 a3 a4 = opcsDep_ "FLsetTextColor" [(Xr, [Ir, Ir, Ir, Ir])] [a1, a2, a3, a4]

{- |
Sets the size of the text label of a FLTK widget.

FLsetTextSize sets the size of the text label of the target widget.

>  FLsetTextSize  isize, ihandle

csound doc: <https://csound.com/docs/manual/FLsetTextSize.html>
-}
flSetTextSize :: D -> D -> SE ()
flSetTextSize b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetTextSize" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Sets some font attributes of the text label of a FLTK widget.

FLsetTextType sets some attributes related to the fonts of the text label of the target widget.

>  FLsetTextType  itype, ihandle

csound doc: <https://csound.com/docs/manual/FLsetTextType.html>
-}
flSetTextType :: D -> D -> SE ()
flSetTextType b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "FLsetTextType" [(Xr, [Ir, Ir])] [a1, a2]

{- |
Restores the visibility of a previously hidden FLTK widget.

FLshow restores the visibility of a previously hidden widget.

>  FLshow  ihandle

csound doc: <https://csound.com/docs/manual/FLshow.html>
-}
flShow :: D -> SE ()
flShow b1 =
  SE $ join $ f <$> (lift . unD) b1
  where
    f a1 = opcsDep_ "FLshow" [(Xr, [Ir])] [a1]
