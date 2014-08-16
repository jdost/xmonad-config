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
show_mpd = True

workspaces' :: [(String, String)]
workspaces' = [("0_1", "1:main"), ("0_2", "2:web"), ("0_3", "3:games"),
  ("0_4", "4:vm"), ("0_5", "")]

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
  , setShifts "0_3" games
  , setShifts "0_4" ["VirtualBox"]
  , setIgnores ignores
  ]

layouts _ = avoidStruts $ smartBorders $ layoutHints
    $ onWorkspace "0_1" (mirror normal ||| Full)
    $ onWorkspace "0_2" (browser || Full)
    $ onWorkspace "0_3" (Full)
    $ onWorkspace "1_2" (Mirror normal ||| Full)
    $ (normal ||| Mirror normal ||| Full)
  where
    nconf   = defaultNormalConf
    normal  = normalLayout nconf
    bconf   = defaultBrowserConf
    browser = browserLayout bconf

tl_dzen :: DzenConf
tl_dzen = defaultDzenConf {
      width = Just 1000
    }

tr_dzen :: Int -> DzenConf
tr_dzen w = defaultDzenConf {
      xPosition = Just 1000
    , width = Just (w - 1000)
    , alignment = Just RightAlign
    }
