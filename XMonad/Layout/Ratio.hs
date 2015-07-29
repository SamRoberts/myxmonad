{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Ratio
-- Copyright   :  (c) Sam Roberts <sam.roberts.1983@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Sam Roberts <sam.roberts.1983@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- The constant ratio spiral layout
--
-- This layout is aimed at making a fibonacci-style spiral
-- without any of the annoying jumps in window size when
-- creating or deleting windows.
--
-- The default ratio is the limit of the fibonacci-style spiral:
-- the golden ratio: (sqrt 5 + 1) / 2
--
-----------------------------------------------------------------------------

module XMonad.Layout.Ratio (
  Ratio(..)
  , goldenRatio
  , ratio
  ) where

import XMonad
import XMonad.StackSet (integrate)
import Graphics.X11 (Rectangle(..))

goldenRatio = Ratio $ (sqrt 5 + 1) / 2
ratio = Ratio

data Ratio a = Ratio { _ratio :: Double }
             deriving (Show, Read)

instance LayoutClass Ratio a where
  description _ = "Ratio"
  
  pureMessage (Ratio ratio) m = fmap resize $ fromMessage m
    where
      resize Shrink = Ratio $ ratio * 19 / 20
      resize Expand = Ratio $ ratio * 20 / 19

  pureLayout (Ratio ratio) r s = layout r West $ integrate s
    where
      layout r d (w:[]) = [(w,r)]
      layout r d (w:ws) = let (rw,r') = split d r
                          in (w,rw) : layout r' (nextFromTo West North d) ws

      split West  (Rectangle x y w h) = let w1 = shrink w
                                            w2 = w - w1
                                            x2 = x + fromIntegral w1
                                        in (Rectangle x y w1 h, Rectangle x2 y w2 h)

      split East  (Rectangle x y w h) = let w1 = shrink w
                                            w2 = w - w1
                                            x1 = x + fromIntegral w2
                                        in (Rectangle x1 y w1 h, Rectangle x y w2 h)

      split North (Rectangle x y w h) = let h1 = shrink h
                                            h2 = h - h1
                                            y2 = y + fromIntegral h1
                                        in (Rectangle x y w h1, Rectangle x y2 w h2)

      split South (Rectangle x y w h) = let h1 = shrink h
                                            h2 = h - h1
                                            y1 = y + fromIntegral h2
                                        in (Rectangle x y1 w h1, Rectangle x y w h2)

      shrink dim = ceiling $ fromIntegral dim * ratio / (ratio + 1)


data Direction = West | North | East | South deriving (Eq, Ord, Enum, Bounded)

next :: (Eq a, Enum a, Bounded a) => a -> a
next = nextFromTo minBound maxBound

nextFromTo :: (Eq a, Enum a) => a -> a -> a -> a
nextFromTo min max x | x == max  = min
                     | otherwise = succ x
