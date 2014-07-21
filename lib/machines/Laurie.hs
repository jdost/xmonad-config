module CurrentMachine where

import Layouts
import Hooks
import Defaults
import KeyBindings
import StatusBars

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)

import qualified Data.Map as M
import Graphics.X11.ExtraTypes

show_mpd :: Bool
show_mpd = False

workspaces' :: [(String, String)]
workspaces' = [("0_1", "1:main"), ("0_2", "2:web"),
  ("0_3", "3:work"), ("0_4", "4:comm"), ("0_5", "")]

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ KeyBindings.extraKeys extraCommands
  ++ KeyBindings.workspaceChanging c
  ++ [
    ((0 , xF86XK_LaunchA), spawn (homeBin ++ "syncmail"))
  ]
  where
      ws = map fst $ workspaces'
      extraCommands = defaultExtraCommands
        { mon_up   = homeBin ++ "mon_brightness up"
        , mon_down = homeBin ++ "mon_brightness down"
        , kbd_up   = homeBin ++ "kbd_brightness up"
        , kbd_down = homeBin ++ "kbd_brightness down"
        }

hooks :: ManageHook
hooks = composeAll . concat $
   [ makeCenter games
   , setIgnores ignores
   , setShifts "0_2" browsers
   ]

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ onWorkspace "0_1" (normal ||| Full)
    $ onWorkspace "0_2" (browser ||| Full)
    $ (normal ||| Mirror normal ||| Full)
  where
    nconf = defaultNormalConf
    normal = normalLayout nconf
    bconf = defaultBrowserConf
    browser = browserLayout bconf

tr_dzen :: DzenConf
tr_dzen = defaultDzenConf {
      xPosition = Just 600
    , width = Just 1000
    , alignment = Just RightAlign
    }

tl_dzen :: DzenConf
tl_dzen = defaultDzenConf {
      width = Just 600
    }
