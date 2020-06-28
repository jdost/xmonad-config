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
  ++ KeyBindings.extraKeys defaultExtraCommands
  ++ KeyBindings.workspaceChanging c

hooks :: ManageHook
hooks = composeAll . concat $
  [ setShifts "0_2" browsers
  , setIgnores ignores
  ]

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ (normal ||| full)
  where
    nconf = defaultNormalConf { ngaps = 50 }
    normal = normalLayout nconf
    full = fullLayout defaultFullConf
