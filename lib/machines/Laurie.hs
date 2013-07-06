module CurrentMachine where

import Layouts

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ onWorkspace "0_1" (Mirror normal ||| Full)
    $ (normal ||| Mirror normal ||| Full)
  where
    nconf = defaultNormalConf
    normal = normalLayout nconf
