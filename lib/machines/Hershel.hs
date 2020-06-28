module CurrentMachine where

import Layouts
import Hooks
import Defaults
import KeyBindings
import StatusBars

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.NamedScratchpad

import qualified Data.Map as M
import Graphics.X11.ExtraTypes

workspaces' :: [String]
workspaces' = ["code", "web", "games", "video", "chat"]

scratchpads' :: [NamedScratchpad]
scratchpads' = [
    NS "spotify" "alacritty --title music-tmux -e ~/.xmonad/etc/music-tmux" (title =? "music-tmux") spotifyFloating
  ]
    where
      spotifyFloating = customFloating $ W.RationalRect x y w h
        where
          w = 0.3 --30% width
          h = 1.0 --100% height
          x = 1 - w --right
          y = 0.0 --top

musicCommands' :: MusicCommands
musicCommands' = defaultMusicCommands
  { next = "playerctl next"
  , prev = "playerctl previous"
  , toggle = "playerctl play-pause"
  }

extraCommands' :: ExtraCommands
extraCommands' = defaultExtraCommands
  { audio_up = "pulsemixer --change-volume +5"
  , audio_down = "pulsemixer --change-volume -5"
  , audio_toggle = "pulsemixier --toggle-mute"
  }

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.multiHeadNavigation [xK_o, xK_i]
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf []
  ++ KeyBindings.musicControl musicCommands'
  ++ KeyBindings.extraKeys extraCommands'
  ++ KeyBindings.workspaceChanging c
  ++ [((m, xK_m), namedScratchpadAction scratchpads' "spotify")]

hooks :: ManageHook
hooks = (composeAll . concat $
  [ setShifts "web" browsers
  , setShifts "games" games
  , setShifts "video"  video
  , setShifts "chat" chats
  , setIgnores ignores
  , makeFloat floats
  , steam
  ]) <+> namedScratchpadManageHook scratchpads'

layouts _ = smartBorders $ avoidStruts
    $ onWorkspace "code" (normal ||| full)
    $ onWorkspace "web" (browser ||| full)
    $ onWorkspace "games" (normal ||| full)
    $ onWorkspace "video" (normal ||| full)
    $ onWorkspace "chat" (full)
    $ (normal ||| Mirror normal ||| full)
  where
    nconf   = defaultNormalConf
    normal  = normalLayout nconf
    bconf   = defaultBrowserConf
    browser = browserLayout bconf
    fconf   = defaultFullConf
    full    = fullLayout fconf
