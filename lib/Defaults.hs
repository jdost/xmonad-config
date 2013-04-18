module Defaults where

import Graphics.X11.Xlib.Types (Dimension)

defaultTerminal :: String
defaultTerminal = "urxvtc"

defaultMouseFocus :: Bool
defaultMouseFocus = False

defaultBorderWidth :: Dimension
defaultBorderWidth = 0

defaultKillCmd :: String
defaultKillCmd = "killall dzen2 trayer conky; xmonad --recompile; xmonad --restart"
