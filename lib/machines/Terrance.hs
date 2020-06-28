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
import qualified XMonad.Prompt as P

import qualified Data.Map as M
import Graphics.X11.Types
import Graphics.X11.ExtraTypes

workspaces' :: [String]
workspaces' = ["1:\xf120", "2:\xe744", "3:\xf47f", "4:\xf0e6", ""]

promptConf' = defaultPromptConf
  { P.font = "xft:UbuntuMono Nerd Font:size=15"
  , P.height = 20
  }

musicCmds :: MusicCommands
musicCmds = defaultMusicCommands
  { next = "playerctl next"
  , prev = "playerctl previous"
  , toggle = "playerctl play-pause"
  }

extraCmds :: ExtraCommands
extraCmds = defaultExtraCommands
  { audio_up = "pulsemixer --change-volume +5"
  , audio_down = "pulsemixer --change-volume -5"
  , audio_toggle = "pulsemixer --toggle-mute"
  }


keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl promptConf' filteredCommands
  ++ KeyBindings.musicControl musicCmds
  ++ KeyBindings.extraKeys extraCmds
  ++ KeyBindings.workspaceChanging c
  ++ [
    ((m , xK_BackSpace), spawn (homeBin ++ "lock"))
  , ((m , xK_Insert), spawn (homeBin ++ "display internal"))
  ]

hooks :: ManageHook
hooks = composeAll . concat $
  [ setShifts "2:\xe744" browsers
  , setShifts "4:\xf0e6" chats
  , setIgnores ignores
  , steam
  ]

layouts _ = smartBorders $ avoidStruts
    $ onWorkspace "2:\xe744" (browser ||| full)
    $ onWorkspace "4:\xf0e6" (browser ||| full)
    $ (normal ||| full)
  where
    nconf = defaultNormalConf { ngaps = 50 }
    normal = normalLayout nconf
    bconf = defaultBrowserConf
    browser = browserLayout bconf
    full = fullLayout defaultFullConf
