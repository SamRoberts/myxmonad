module Main (main) where

import System.Environment (getArgs)

import Graphics.X11.Types (mod4Mask, xK_B)

import XMonad (XConfig(..), (|||), defaultConfig, spawn, terminal, xmonad)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP, statusBar)
import XMonad.Layout.Ellipse (ellipse)
import XMonad.Layout.Ratio  (goldenRatio)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Util.EZConfig (additionalKeysP)

main = do
  [xmobar] <- getArgs
  conf <- statusBar xmobar myOutput (const (myModMask, xK_B)) myConfig
  xmonad conf

myConfig = defaultConfig
           { logHook = fadeInactiveLogHook 0.6
           , layoutHook = ellipse ||| goldenRatio ||| simpleTabbed
           , terminal = myTerminal
           , modMask = myModMask
           }
           `additionalKeysP`
           [ ("M-r f", spawn "firefox")
           , ("M-r e", spawn "emacs")
           , ("M-r t", spawn myTerminal)
           , ("M-S-.", spawn "bgnext.sh")
           , ("M-S-,", spawn "bgprev.sh")
           ]

myTerminal = "urxvt +sb -vb -fg White -bg Black"

myOutput = defaultPP
           { ppTitle = const ""
           , ppLayout = const ""
           }

myModMask = mod4Mask
