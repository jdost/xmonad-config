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
import Graphics.X11.ExtraTypes

show_mpd :: Bool
show_mpd = False

barHeight :: Maybe Int
barHeight = Just 22

workspaces' :: [String]
workspaces' = ["1:\xf120", "2:\xe744", "3:\xf47f", "4:\xf0e6", ""]

promptConf' = defaultPromptConf
  { P.font = "xft:UbuntuMono Nerd Font:size=15"
  , P.height = 33
  }

extraCommands = defaultExtraCommands
  { audio_up = "pamixer -i 5"
  , audio_down = "pamixer -d 5"
  , audio_toggle = "pamixer -t"
  }

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl promptConf' filteredCommands
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ KeyBindings.extraKeys extraCommands
  ++ KeyBindings.workspaceChanging c

hooks :: ManageHook
hooks = composeAll . concat $
  [ setShifts "2:\xe744" browsers
  , setShifts "3:\xf47f" games
  , setShifts "4:\xf0e6" chats
  , setIgnores ignores
  , makeCenter floats
  , makeCenter ["FTL"]
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

tr_dzen :: Int -> DzenConf
tr_dzen w = defaultDzenConf {
      xPosition = Just 700
    , width = Just (w - 700)
    , height = barHeight
    , alignment = Just RightAlign
    }

tl_dzen :: DzenConf
tl_dzen = defaultDzenConf {
      width = Just 700
    , height = barHeight
    }
