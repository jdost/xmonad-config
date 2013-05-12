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

defaultMHKeys :: [KeySym]
defaultMHKeys = [xK_w, xK_e, xK_r]
