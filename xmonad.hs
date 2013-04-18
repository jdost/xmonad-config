import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(NoUrgencyHook) )
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Actions.UpdatePointer (updatePointer, PointerPosition(Relative) )

import qualified Data.Map as M
import Graphics.X11.ExtraTypes
import System.Posix.Unistd (getSystemID, nodeName)

import Colors as C
import Defaults
import Hooks
import KeyBindings
import Layouts
import StatusBars

workspaces' :: String -> [(String, String)]
workspaces' "Laurie" = [("0_1", "1:main"), ("0_2", "2:web"),
  ("0_3", "3:work"), ("0_4", "4:comm"), ("0_5", "")]

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
  ++ KeyBindings.xmonadBasics defaultKillCmd
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

layouts "Laurie" = avoidStruts $ smartBorders $ layoutHints (normal ||| Mirror normal ||| Full)
  where
    nconf = defaultNormalConf
    normal = normalLayout nconf

layoutAliases :: [(String, String)]
layoutAliases =
  [ ("Hinted Spacing 4 ResizableTall", " RT")
  , ("Hinted Mirror Spacing 4 ResizableTall", "MRT")
  , ("Hinted Full", " F ")
  ]

hooks :: String -> ManageHook
hooks "Laurie" = composeAll . concat $
   [ makeCenter games
   , setIgnores ignores
   , setShifts "0_2" browsers
   ]

conky_loc :: String
conky_loc = "~/.xmonad/conky/"

main = do
  -- get hostname, use to distinguish systems
  hostname <- getHostname
  -- spawn dzen2 bars
  spawnPipe $ conkyDzen (conky_loc ++ "tr_main.conky") defaultDzenConf {
      xPosition = Just 600
    , width = Just 1000
    , alignment = Just RightAlign
    }
  spawnPipe $ conkyDzen (conky_loc ++ "br_main." ++ hostname ++ ".conky") defaultDzenConf {
      yPosition = Just 1200
    , width = Just 2000
    , alignment = Just RightAlign
    }
  dh_dzen <- spawnPipe $ dzen defaultDzenConf {
      width = Just 600
    }
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
