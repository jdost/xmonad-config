module StatusBars (
    DzenConf (..)
  , TextAlignDzen (..)
  , ScreenNum
  , TrayConf (..)
  , VTextAlignTray (..)
  , HTextAlignTray (..)

  , defaultDzenConf
  , dzen

  , defaultDHConf
  , conkyDzen

  , defaultTrayConf
  , tray

  , getScreenWidth
  ) where

import Data.Char (chr)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.IO (Handle, hPutStrLn)
import XMonad.Hooks.DynamicLog hiding (dzen)
import qualified Colors

import Graphics.X11.Xlib (openDisplay, rect_width)
import Graphics.X11.Xinerama (getScreenInfo)

data DzenConf = DzenConf
  { xPosition :: Maybe Int
  , yPosition :: Maybe Int
  , screen    :: Maybe ScreenNum
  , width     :: Maybe Int
  , height    :: Maybe Int
  , alignment :: Maybe TextAlignDzen
  , font      :: Maybe String
  , fgColor   :: Maybe String
  , bgColor   :: Maybe String
  }

type ScreenNum = Int
data TextAlignDzen = LeftAlign | RightAlign | Centered
instance Show TextAlignDzen where
  show LeftAlign  = "l"
  show RightAlign = "r"
  show Centered   = "c"

_ubuntuMonoFont :: String
_ubuntuMonoFont = "-*-ubuntu mono-medium-r-normal-*-11-*-*-*-*-*-*-*"

defaultDzenConf :: DzenConf
defaultDzenConf = DzenConf
  { xPosition = Just 0
  , yPosition = Just 0
  , screen    = Just 0
  , width     = Nothing
  , height    = Just 15
  , alignment = Just LeftAlign
  , font      = Nothing
  , fgColor   = Just Colors.dzenFG
  , bgColor   = Just Colors.dzenBG
  }

colorDelim :: Char
colorDelim = chr 127
colorDivider :: Char
colorDivider = '|'

defaultDHConf :: Handle -> String -> [(String, String)] -> [(String, String)] -> PP
defaultDHConf h screen' wslist layouts = defaultPP
  { ppCurrent = wrapWS Colors.focusedWS . (\x -> dropWS x)
  , ppVisible = wrapWS Colors.unfocusedWS . (\x -> dropWS x)
  , ppHidden = wrapWS Colors.hiddenWS . (\x -> dropWS x)
  , ppHiddenNoWindows = wrapWS Colors.emptyWS . (\x -> dropWS x)
  , ppUrgent = wrapWS Colors.urgentWS . (\x -> dropWS x)
  , ppSep = [colorDivider]
  , ppWsSep = [colorDivider]
  , ppTitle = wrapTitle Colors.activeTitle
  , ppOrder = \(ws:l:t:_) -> [l,ws,t]
  , ppLayout = wrapLayout Colors.activeLayout
  , ppOutput = (\x -> hPutStrLn h $ cleanStatus x)
  }
  where
    cleanStatus s = concat $ map filterDividers $ strSplit colorDelim s
    wrapWS _ "" = ""
    wrapWS (fg,bg) name = (postBG bg) ++ (makeFG fg) ++ " " ++ name ++ " " ++ (preBG bg)
    wrapLayout (fg,bg) name = (makeBG bg) ++ (makeFG fg) ++ " " ++
        (lookup' name $ trans layouts name) ++ " " ++ (preBG bg)
    wrapTitle (fg,bg) name = (postBG bg) ++ (makeFG fg) ++ "  " ++ name
    postBG bg = bg ++ [colorDelim]
    preBG bg = [colorDelim] ++ bg
    makeFG fg = "^fg(" ++ fg ++ ")"
    makeBG bg = "^bg(" ++ bg ++ ")"
    trans table item = lookup item table
    lookup' orig trans'
      | trans' == Nothing = orig
      | otherwise = fromMaybe "" trans'
    dropWS ws
      | screen' `isPrefixOf` ws = lookup' ws $ trans wslist ws
      | otherwise = ""

dzen :: DzenConf -> String
dzen c = unwords $ ["dzen2"]
    ++ addArg ("-fn", fmap quote $ font c)
    ++ addArg ("-fg", fmap quote $ fgColor c)
    ++ addArg ("-bg", fmap quote $ bgColor c)
    ++ addArg ("-ta", fmap show $ alignment c)
    ++ addArg ("-x",  fmap show $ xPosition c)
    ++ addArg ("-y",  fmap show $ yPosition c)
    ++ addArg ("-w",  fmap show $ width c)
    ++ addArg ("-h",  fmap show $ height c)
    ++ addArg ("-xs", fmap show $ fmap (+1) $ screen c)
  where
    quote = ("'" ++ ) . ( ++ "'")
    addArg (_, Nothing) = []
    addArg (opt, Just val) = [opt, val]

conkyDzen :: String -> DzenConf -> String
conkyDzen "" _ = ""
conkyDzen conkyrc dzenconfig = conky ++ " | " ++ (dzen dzenconfig)
  where
    conky = "conky -c " ++ conkyrc

data TrayConf = TrayConf
  { edge :: Maybe VTextAlignTray
  , hAlignment :: Maybe HTextAlignTray
  , pStrut :: Maybe Bool
  , height' :: Maybe Int
  , tint :: Maybe String
  , transparent :: Maybe Bool
  , expand :: Maybe Bool
  , distance :: Maybe Int
  }

tray :: TrayConf -> String
tray conf = unwords $ ["trayer"]
  ++ addArg ("--edge", fmap show $ edge conf)
  ++ addArg ("--SetPartialStrut", fmap show $ pStrut conf)
  ++ addArg ("--align", fmap show $ hAlignment conf)
  ++ addArg ("--height", fmap show $ height' conf)
  ++ ["--widthtype", "request"]
  ++ addArg ("--tint", fmap show $ tint conf)
  ++ addArg ("--transparent", fmap show $ transparent conf)
  ++ addArg ("--expand", fmap show $ expand conf)
  ++ addArg ("--distance", fmap show $ distance conf)
  where
    addArg (_, Nothing) = []
    addArg (opt, Just val) = [opt, val]

defaultTrayConf :: TrayConf
defaultTrayConf = TrayConf
  { edge = Just TopAlign
  , hAlignment = Just RightAlign'
  , pStrut = Just False
  , height' = Just 16
  , tint = Just "0x333333"
  , transparent = Just True
  , expand = Just True
  , distance = Just 15
  }

data VTextAlignTray = TopAlign | BottomAlign
instance Show VTextAlignTray where
  show TopAlign = "top"
  show BottomAlign = "bottom"
data HTextAlignTray = LeftAlign' | RightAlign'
instance Show HTextAlignTray where
  show LeftAlign' = "left"
  show RightAlign' = "right"

-- DynamicHook fixers
mkSolidDivider :: String -> String -> Int -> String
mkSolidDivider _ _ 0 = ""
mkSolidDivider fg bg n = colorLead $ concat $ map mkRect $ map scaleSizes sizes
  where
    sizes = [0..(n-1)]
    scaleSizes = (\x -> n*3 - x*3)
    mkRect = (\x -> "^r(1x" ++ show (x) ++ ")")
    colorLead rects = "^pa(;0)^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ rects ++ "^pa(;2)"

mkLineDivider :: String -> Int -> String
mkLineDivider _ 0 = ""
mkLineDivider color n = colorLine $ concat $ map mkRect $ map scaleSizes sizes
  where
    sizes = [0..(n-1)]
    scaleSizes = (\x -> (n-1)*3 - x*3)
    mkRect = (\x -> "^pa(;" ++ show x ++ ")^r(1x3)")
    colorLine rects = "^fg(" ++ color ++ ")" ++ rects ++ "^pa(;2)"

strSplit :: Char -> String -> [String]
strSplit _ "" = []
strSplit c str = case dropWhile (== c) str of
    "" -> []
    str' -> w : strSplit c str''
        where (w, str'') = break (== c) str'

filterDividers :: String -> String
filterDividers str
  | length str == 15 && [colorDivider] `isInfixOf` str = makeDivider $ strSplit colorDivider str
  | otherwise = str
  where
    makeDivider (fg:bg:_)
      | fg /= bg = mkSolidDivider fg bg 5
      | otherwise = mkLineDivider "#666666" 5

getScreenWidth :: IO Int
getScreenWidth = do
  display <- openDisplay ""
  screen <- getScreenInfo display
  return $ fromIntegral $ rect_width $ head $ screen
