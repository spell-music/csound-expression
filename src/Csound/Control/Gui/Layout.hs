-- | The functions from this module specify the geometry 
-- of the GUI-elements. They tell where to render the elements.
--
-- Every element is rectangular. To know where to place the element is 
-- to know the parameters of the bounding rectangle. All rectangles are
-- relative and automatically aligned. 
--
-- We have two functions for grouping. They construct horizontal and vertical
-- groups of the elements. Within the group we can change the relative size 
-- of the rectangles (by scaling one side of the rectangle). In place of rectangle
-- we can put an empty space. 
module Csound.Control.Gui.Layout (
    hor, ver, space, sca, horSca, verSca, grid,
    padding, margin, resizeGui,
) where

import Csound.Typed.Gui


grid :: Int -> [Gui] -> Gui
grid columnSize guis = ver $ fmap hor $ splitList columnSize guis
    where
        splitList n xs = case splitAt n xs of
            (res, []) -> [res ++ spaceTail xs]
            (as,rest) -> as : splitList n rest

        spaceTail xs = replicate n space
            where n = getMissingToEven (length xs)

        getMissingToEven total =  case total `mod` columnSize of
            0 -> 0
            n -> columnSize - n

