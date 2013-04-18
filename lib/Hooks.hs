module Hooks (
    browsers
  , games
  , video
  , floats
  , ignores

  , makeHook
  , makeFloat
  , makeCenter
  , setIgnores
  , setShifts
  ) where

import XMonad ( Query )
import XMonad.Core (WindowSet)
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import Data.Monoid (Monoid, Endo)
-- Windows that are classified as browsers
browsers :: [String]
browsers = ["Google-chrome", "Chromium", "Dwb", "Firefox", "Chrome"]
-- Windows that are classified as games
games :: [String]
games = ["Dwarf_Fortress", "Osmos",
    "com-threerings-yohoho-client-YoApp",
    "net-minecraft-MinecraftLauncher",
    -- emulators
    "dosbox", "VisualBoyAdvance"]
-- Windows that are classified as video (for workspace moving)
video :: [String]
video = ["mplayer", "Vlc"]
-- Windows that automatically float
floats :: [String]
floats = ["Wine"]
-- Windows that get ignored by the focus loop
ignores :: [String]
ignores = ["trayer"]

-- generic wrapper for the list comprehension
makeHook :: (Eq a, Monoid b) => Query b -> Query a -> [a] -> [Query b]
makeHook action matchType set = [ matchType =? s --> action | s <- set ]

type MHReturn = [Query (Endo WindowSet)]
makeFloat :: ([String] -> MHReturn)
makeFloat = makeHook doFloat className

makeCenter :: ([String] -> MHReturn)
makeCenter = makeHook doCenterFloat className

setIgnores :: [String] -> MHReturn
setIgnores ignores' = makeHook doIgnore resource (ignores' ++ ignores)

setShifts :: String -> ([String] -> MHReturn)
setShifts ws = makeHook (doShift ws) className

