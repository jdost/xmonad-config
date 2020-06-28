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

workspaces' :: [String]
workspaces' = ["1:main", "2:web", "3:games", "4:kim", ""]

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.multiHeadNavigation [xK_o, xK_i]
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf filteredCommands
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ KeyBindings.extraKeys defaultExtraCommands
  ++ KeyBindings.workspaceChanging c

hooks :: ManageHook
hooks = composeAll . concat $
  [ setShifts "2:web" ["qutebrowser"]
  , setShifts "3:games" games
  , setShifts "4:kim" ["Chromium"]
  , setIgnores ignores
  , steam
  ]

layouts _ = smartBorders $ avoidStruts
    $ onWorkspace "1:main" (normal ||| full)
    $ onWorkspace "2:web" (browser ||| full)
    $ onWorkspace "3:games" (full)
    $ onWorkspace "4:kim" (full)
    $ (normal ||| Mirror normal ||| full)
  where
    nconf   = defaultNormalConf
    normal  = normalLayout nconf
    bconf   = defaultBrowserConf
    browser = browserLayout bconf
    fconf   = defaultFullConf
    full    = fullLayout fconf
