import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(NoUrgencyHook) )
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Actions.UpdatePointer (updatePointer, PointerPosition(Relative) )

import qualified Data.Map as M
import Data.List (transpose)
import Graphics.X11.Types (Window)
import Graphics.X11.ExtraTypes
import Graphics.X11.Xlib (openDisplay)
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
      yPosition = Just 1200
    , width = Just 900
    }
spawn_mpd _ = ""

main = do
  -- get hostname, use to distinguish systems
  hostname <- getHostname
  display <- openDisplay ""
  screens <- countScreens
  -- spawn dzen2 bars
  spawnPipe $ conkyDzen (conkyFolder ++ "tr_main.conky") tr_dzen
  spawnPipe $ conkyDzen (conkyFolder ++ "br_main." ++ hostname ++ ".conky") defaultDzenConf {
      yPosition = Just 1200
    , xPosition = if show_mpd then Just 500 else Just 0
    , width = if show_mpd then Just 1500 else Just 2000
    , alignment = Just RightAlign
    }
  spawnPipe $ (spawn_mpd show_mpd)
  dh_dzen <- spawnPipe $ dzen tl_dzen
  dh_dzen2 <- spawnPipe $ dzen (defaultDzenConf {
    width = Just 1000
  , screen = Just 1
  })
  spawnPipe $ tray defaultTrayConf
  -- make xmonad
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal = defaultTerminal
    , focusFollowsMouse = defaultMouseFocus

    , borderWidth = defaultBorderWidth
    , normalBorderColor = C.unfocusedBorder
    , focusedBorderColor = C.focusedBorder

    , modMask = m
    , workspaces = map fst workspaces'
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
