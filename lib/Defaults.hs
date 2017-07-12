module Defaults where

import Graphics.X11.Xlib.Types (Dimension)
import Graphics.X11.Types

defaultTerminal :: String
defaultTerminal = "urxvtc"

defaultMouseFocus :: Bool
defaultMouseFocus = False

defaultBorderWidth :: Dimension
defaultBorderWidth = 4

defaultKillCmd :: String
defaultKillCmd = "killall dzen2 trayer conky; xmonad --recompile; xmonad --restart"

defaultLockCmd :: String
defaultLockCmd = "ncmpcpp pause; xscreensaver-command --lock"

defaultMHKeys :: [KeySym]
defaultMHKeys = [xK_w, xK_e, xK_r]

homeBin :: String
homeBin = "~/.local/bin/"

conkyFolder :: String
conkyFolder = "~/.xmonad/conky/"

filteredCommands :: [String]
filteredCommands = [
    "chromedriver"
  , "chroot" , "chrt"
  , "steam-native" , "steam-runtime" , "steam-deps"
  , "slabtop"
  , "yacc"
  ]
