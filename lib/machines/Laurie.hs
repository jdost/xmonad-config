module CurrentMachine where

import Layouts

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)

show_mpd :: Bool
show_mpd = False

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ onWorkspace "0_1" (normal ||| Full)
    $ onWorkspace "0_2" (browser ||| Full)
    $ (normal ||| Mirror normal ||| Full)
  where
    nconf = defaultNormalConf
    normal = normalLayout nconf
    bconf = defaultBrowserConf
    browser = browserLayout bconf
