import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(NoUrgencyHook) )
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Actions.UpdatePointer (updatePointer, PointerPosition(Relative) )

import qualified Data.Map as M
import Data.List (transpose)
import Graphics.X11.Types (Window)
import Graphics.X11.ExtraTypes
import Graphics.X11.Xlib (openDisplay)
import Graphics.X11.Xinerama (xineramaQueryScreens)
import System.Posix.Unistd (getSystemID, nodeName)

import Colors as C
import Defaults
import Hooks
import KeyBindings
import StatusBars

import CurrentMachine

workspaces' :: String -> [(String, String)]
workspaces' "Laurie" = [("0_1", "1:main"), ("0_2", "2:web"),
  ("0_3", "3:work"), ("0_4", "4:comm"), ("0_5", "")]
workspaces' "Zito" = concat $ transpose [
    [("0_1", "1:main"), ("0_2", "2:web"), ("0_3", "3:games"), ("0_4", "4:vm"),
     ("0_5", "")],
    [("1_1", "1:irc"), ("1_2", "2:mon"), ("1_3", "3:misc"),
     ("1_4", ""), ("1_5", "")]
  ]
workspaces' "Fernando" = concat $ transpose [
    [("0_1", "1:main"), ("0_2", "2:web"), ("0_3", "3:code"), ("0_4", "4:vm")]
  , [("1_1", "1:irc"), ("1_2", "2:im"), ("1_3", "3:code"), ("1_4", "4:mon")]
  ]

home_bin :: String
home_bin = "~/.bin/"
_ExtraCommands :: String -> ExtraCommands
_ExtraCommands "Laurie" = defaultExtraCommands
  { mon_up   = home_bin ++ "mon_brightness up"
  , mon_down = home_bin ++ "mon_brightness down"
  , kbd_up   = home_bin ++ "kbd_brightness up"
  , kbd_down = home_bin ++ "kbd_brightness down"
  }

keys' :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' "Laurie" c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ (KeyBindings.extraKeys (_ExtraCommands "Laurie"))
  ++ KeyBindings.workspaceChanging c
  ++ [
    ((0 , xF86XK_LaunchA), spawn (home_bin ++ "syncmail"))
  ]
  where
      ws = map fst $ workspaces' "Laurie"
keys' "Zito" c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ KeyBindings.extraKeys defaultExtraCommands
  ++ KeyBindings.workspaceChanging c
  ++ KeyBindings.multiHeadNavigation defaultMHKeys
keys' "Fernando" c = M.fromList $ []
  ++ KeyBindings.xmonadBasics defaultKillCmd defaultLockCmd
  ++ KeyBindings.windowNavigation
  ++ KeyBindings.windowSizing
  ++ KeyBindings.layoutControl
  ++ KeyBindings.processControl defaultPromptConf
  ++ KeyBindings.musicControl defaultMusicCommands
  ++ KeyBindings.extraKeys defaultExtraCommands
  ++ KeyBindings.workspaceChanging c
  ++ KeyBindings.multiHeadNavigation defaultMHKeys

layoutAliases :: [(String, String)]
layoutAliases =
  [ ("Hinted Spacing 4 ResizableTall", " RT")
  , ("Hinted Mirror Spacing 4 ResizableTall", "MRT")
  , ("Hinted Full", " F ")
  , ("Hinted Spacing 4 TwoPane", " 2P")
  , ("Hinted Spacing 4 IM Grid", " IM")
  ]

hooks :: String -> ManageHook
hooks "Laurie" = composeAll . concat $
   [ makeCenter games
   , setIgnores ignores
   , setShifts "0_2" browsers
   ]
hooks "Zito" = composeAll . concat $
  [ setShifts "0_2" browsers
  , setShifts "0_3" games
  , setShifts "0_4" ["VirtualBox"]
  , setIgnores ignores
  ]
hooks "Fernando" = composeAll . concat $
  [ setShifts "0_2" browsers
  , setShifts "0_3" games
  , setShifts "0_4" ["VirtualBox"]
  , setIgnores ignores
  ]

conky_loc :: String
conky_loc = "~/.xmonad/conky/"

tr_dzen :: String -> DzenConf
tr_dzen "Laurie" = defaultDzenConf {
      xPosition = Just 600
    , width = Just 1000
    , alignment = Just RightAlign
    }
tr_dzen "Zito" = defaultDzenConf {
      xPosition = Just 1000
    , width = Just 920
    , alignment = Just RightAlign
    }
tr_dzen "Fernando" = defaultDzenConf {
      xPosition = Just 1000
    , width = Just 920
    , alignment = Just RightAlign
    }

tl_dzen :: String -> DzenConf
tl_dzen "Laurie" = defaultDzenConf {
      width = Just 600
    }
tl_dzen "Zito" = defaultDzenConf {
      width = Just 1000
    }
tl_dzen "Fernando" = defaultDzenConf {
      width = Just 1000
    }

spawn_mpd :: Bool -> String
spawn_mpd True = conkyDzen (conky_loc ++ "bl_main.conky") defaultDzenConf {
      yPosition = Just 1200
    , width = Just 900
    }
spawn_mpd _ = ""

main = do
  -- get hostname, use to distinguish systems
  hostname <- getHostname
  display <- openDisplay ""
  screens <- xineramaQueryScreens display
  -- spawn dzen2 bars
  spawnPipe $ conkyDzen (conky_loc ++ "tr_main.conky") (tr_dzen hostname)
  spawnPipe $ conkyDzen (conky_loc ++ "br_main." ++ hostname ++ ".conky") defaultDzenConf {
      yPosition = Just 1200
    , xPosition = if show_mpd then Just 500 else Just 0
    , width = if show_mpd then Just 1500 else Just 2000
    , alignment = Just RightAlign
    }
  spawnPipe $ (spawn_mpd show_mpd)
  dh_dzen <- spawnPipe $ dzen (tl_dzen hostname)
  spawnPipe $ tray defaultTrayConf
  -- make xmonad
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal = defaultTerminal
    , focusFollowsMouse = defaultMouseFocus

    , borderWidth = defaultBorderWidth
    , normalBorderColor = C.unfocusedBorder
    , focusedBorderColor = C.focusedBorder

    , modMask = m
    , workspaces = map fst (workspaces' hostname)
    , keys = keys' hostname
    , layoutHook = layouts hostname
    , manageHook = manageHook defaultConfig <+> hooks hostname <+> manageDocks

    , logHook = do
        dynamicLogWithPP $ defaultDHConf dh_dzen "0" (workspaces' hostname) layoutAliases
        fadeInactiveLogHook 1.0
    }

getHostname :: IO String
getHostname = do
  host <- getSystemID
  return $ nodeName host
-- vim: ft=haskell
