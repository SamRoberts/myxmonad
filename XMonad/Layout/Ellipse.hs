{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Circle
-- Copyright   :  (c) Peter De Wachter
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Circle is an elliptical, overlapping layout, by Peter De Wachter
--
--
-- NOTE: This circle layout has some changes by Sam Roberts
--       should move to my own module, or try to get changes 
--       in default module
-----------------------------------------------------------------------------

module XMonad.Layout.Ellipse (
  -- * Usage
  -- $usage
  ellipse
  , Ellipse (..)
  ) where

import Data.List
import XMonad
import XMonad.StackSet (integrate, peek)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Ellipse
--
-- Then edit your @layoutHook@ by adding the Circle layout:
--
-- > myLayout = ellipse ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

ellipse = Ellipse $ 1 / sqrt 2

data Ellipse a = Ellipse Double deriving ( Read, Show )

instance LayoutClass Ellipse Window where
  description _ = "Ellipse"
    
  pureMessage (Ellipse ratio) m = fmap resize $ fromMessage m
    where
      resize Shrink = Ellipse $ ratio * 19 / 20
      resize Expand = Ellipse $ ratio * 20 / 19

  doLayout (Ellipse ratio) r s =
    do layout <- raiseFocus $ ellipseLayout ratio r $ integrate s
       return (layout, Nothing)

ellipseLayout :: Double -> Rectangle -> [a] -> [(a, Rectangle)]
ellipseLayout ratio _ []     = []
ellipseLayout ratio r (w:ws) = master : rest
    where master = (w, center ratio r)
          rest   = zip ws $ map (satellite r) [0, pi * 2 / fromIntegral (length ws) ..]

raiseFocus :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
raiseFocus xs = do focused <- withWindowSet (return . peek)
                   return $ case find ((== focused) . Just . fst) xs of
                              Just x  -> x : delete x xs
                              Nothing -> xs

center :: Double -> Rectangle -> Rectangle
center ratio (Rectangle sx sy sw sh) = Rectangle x y w h
    where w = round (fromIntegral sw * ratio)
          h = round (fromIntegral sh * ratio)
          x = sx + fromIntegral (sw - w) `div` 2
          y = sy + fromIntegral (sh - h) `div` 2

satellite :: Rectangle -> Double -> Rectangle
satellite (Rectangle sx sy sw sh) a = Rectangle (sx + round (rx + rx * cos a))
                                                (sy + round (ry + ry * sin a))
                                                w h
    where rx = fromIntegral (sw - w) / 2
          ry = fromIntegral (sh - h) / 2
          w = sw * 10 `div` 25
          h = sh * 10 `div` 25
