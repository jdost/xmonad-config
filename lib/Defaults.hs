module Defaults where

import Graphics.X11.Xlib.Types (Dimension)
import Graphics.X11.Types

defaultTerminal :: String
defaultTerminal = "urxvtc"

defaultMouseFocus :: Bool
defaultMouseFocus = False

defaultBorderWidth :: Dimension
defaultBorderWidth = 0

defaultKillCmd :: String
defaultKillCmd = "killall dzen2 trayer conky; xmonad --recompile; xmonad --restart"

defaultLockCmd :: String
defaultLockCmd = "ncmpcpp pause; xscreensaver-command --lock"

defaultMHKeys :: [KeySym]
defaultMHKeys = [xK_w, xK_e, xK_r]

homeBin :: String
homeBin = "~/.bin/"

conkyFolder :: String
conkyFolder = "~/.xmonad/conky/"
