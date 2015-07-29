module Main (main) where

import XMonad (XConfig(..), (|||), defaultConfig, spawn, terminal, xmonad)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP)
import XMonad.Layout.Ellipse (ellipse)
import XMonad.Layout.Ratio  (goldenRatio)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Util.EZConfig (additionalKeysP)

main = xmonad myConfig

myConfig = defaultConfig
           { logHook = fadeInactiveLogHook 0.6 >> dynamicLogWithPP myOutput
           , layoutHook = ellipse ||| goldenRatio ||| simpleTabbed
           , terminal = myTerminal
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
