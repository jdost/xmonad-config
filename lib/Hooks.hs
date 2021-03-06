module Hooks (
    browsers
  , games
  , video
  , floats
  , ignores
  , steam
  , chats

  , makeHook
  , makeFloat
  , makeCenter
  , setIgnores
  , setShifts
  ) where

import XMonad ( Query )
import XMonad.Core (WindowSet)
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat)

import Data.Monoid (Monoid, Endo)
-- Windows that are classified as browsers
browsers :: [String]
browsers = ["Google-chrome", "Chromium", "Dwb", "Firefox", "Chrome", "qutebrowser"]
-- Windows that are classified as games
games :: [String]
games = ["Dwarf_Fortress", "Osmos", "FTL",
    "com-threerings-yohoho-client-YoApp",
    "net-minecraft-MinecraftLauncher", "multimc",
    "steam", "Steam",
    "lutris", "Lutris",
    -- emulators
    "Snes9x", "Snes9x-gtk", "Snes",
    "dosbox", "VisualBoyAdvance"]
-- Windows that are classified as video (for workspace moving)
video :: [String]
video = ["mplayer", "mpv", "Vlc", "streamlink-twitch-gui"]
-- Windows that automatically float
floats :: [String]
floats = ["Wine", "Pinentry",
    "Snes9x", "Snes9x-gtk", "Snes",
    "yubioath-desktop", "Yubico Authenticator",
    "Peek"]
-- Windows that get ignored by the focus loop
ignores :: [String]
ignores = ["trayer"]
-- Windows that get classified as chat/communication
chats :: [String]
chats = ["yakyak", "YakYak",
    "slack", "scudcloud", "ScudCloud", "ScudCloud Slack",
    "twitch", "Twitch", "discord", "Discord"]

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

steam :: MHReturn
steam = makeHook doFullFloat className ["steam"]
