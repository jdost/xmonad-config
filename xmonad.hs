import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (manageDocks, docks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(NoUrgencyHook) )
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Actions.UpdatePointer (updatePointer)

import qualified Data.Map as M
import Data.List (transpose)
import Graphics.X11.Types (Window)
import Graphics.X11.ExtraTypes
import System.Posix.Unistd (getSystemID, nodeName)

import Control.Monad (when)

import Colors as C
import Layouts (layoutAliases)
import Defaults
import KeyBindings
import StatusBars

import CurrentMachine

spawn_mpd :: Bool -> String
spawn_mpd True = conkyDzen (conkyFolder ++ "bl_main.conky") defaultDzenConf {
      yPosition = Just 3200
    , height = barHeight
    , width = Just 900
    }
spawn_mpd _ = ""

main = do
  -- get hostname, use to distinguish systems
  hostname <- getHostname
  width <- getScreenWidth
  -- spawn dzen2 bars
  spawnPipe $ conkyDzen (conkyFolder ++ "tr_main.conky") $ tr_dzen width
  spawnPipe $ conkyDzen (conkyFolder ++ "br_main." ++ hostname ++ ".conky") defaultDzenConf {
      yPosition = Just 3200
    , xPosition = if show_mpd then Just 900 else Just 0
    , width = if show_mpd then Just (width - 900) else Just width
    , height = barHeight
    , alignment = Just RightAlign
    }
  spawnPipe $ (spawn_mpd show_mpd)
  dh_dzen <- spawnPipe $ dzen tl_dzen
  spawnPipe $ tray defaultTrayConf {
      distance = barHeight
    , height' = barHeight
  }
  spawnPipe $ notifier defaultNotificationConf
  -- make xmonad
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ docks defaultConfig
    { terminal = defaultTerminal
    , focusFollowsMouse = defaultMouseFocus

    , borderWidth = defaultBorderWidth
    , normalBorderColor = C.unfocusedBorder
    , focusedBorderColor = C.focusedBorder

    , modMask = m
    , workspaces = workspaces'
    , keys = keys'
    , layoutHook = layouts ""
    , manageHook = manageHook defaultConfig <+> hooks <+> manageDocks

    , logHook = do
        dynamicLogWithPP $ defaultDHConf dh_dzen "0" workspaces' layoutAliases
    }

getHostname :: IO String
getHostname = do
  host <- getSystemID
  return $ nodeName host
-- vim: ft=haskell
