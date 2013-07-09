module CurrentMachine where

import Layouts

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)

show_mpd :: Bool
show_mpd = True

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ onWorkspace "0_1" (normal ||| Full)
    $ onWorkspace "1_2" (im)
    $ onWorkspace "1_4" (Mirror normal ||| Full)
    $ (normal ||| Mirror normal ||| Full)
  where
    nconf = defaultNormalConf
    normal = normalLayout nconf
    imconf = defaultIMConf
    im = imLayout imconf
