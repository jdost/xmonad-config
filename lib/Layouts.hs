module Layouts (
    NormalConf (..)
  , defaultNormalConf
  , normalLayout

  , BrowserConf (..)
  , defaultBrowserConf
  , browserLayout

  , FullConf (..)
  , defaultFullConf
  , fullLayout

  , IMConf (..)
  , defaultIMConf
  , imLayout
  , layoutAliases
  ) where

import XMonad.Layout
import XMonad.Core (LayoutClass)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

layoutAliases :: [(String, String)]
layoutAliases =
  [ ("Spacing ResizableTall", " RT")
  , ("Mirror Spacing ResizableTall", "MRT")
  , ("Full", " F ")
  , ("Spacing TwoPane", " 2P")
  , ("Spacing IM Grid", " IM")
  ]
  where
    s = (show _space)

_space :: Integer
_space = 10

_gaps :: Int
_gaps = 30

data NormalConf = NormalConf
  { nmaster  :: Int
  , ndelta   :: Rational
  , nratio   :: Rational
  , nspace   :: Integer
  , ngaps    :: Int
  }

defaultNormalConf :: NormalConf
defaultNormalConf = NormalConf
  { nmaster = 1
  , ndelta = 1/2
  , nratio = 3/100
  , nspace = _space
  , ngaps = _gaps
  }

--_spacing :: int => ModifiedLayout
_spacing x = spacingRaw False (Border 0 0 0 0) False (Border x x x x) True

--normalLayout :: (LayoutClass l a) => NormalConf -> ModifiedLayout Spacing ResizableTall a
normalLayout c = _spacing s $ gaps g $ ResizableTall m r d []
  where
    s = (nspace c)
    m = (nmaster c)
    d = (ndelta c)
    r = (nratio c)
    gapsize = (ngaps c)
    g = [(U, gapsize), (D, gapsize), (R, gapsize), (L, gapsize)]

data BrowserConf = BrowserConf
  { bdelta :: Rational
  , bratio :: Rational
  , bspace :: Integer
  , bgaps  :: Int
  }

defaultBrowserConf :: BrowserConf
defaultBrowserConf = BrowserConf
  { bdelta = 3/100
  , bratio = 1/2
  , bspace = _space
  , bgaps = _gaps
  }
-- TwoPane delta ratio
--browserLayout :: (LayoutClass l a) => BrowserConf -> ModifiedLayout Spacing TwoPane a
browserLayout c = _spacing s $ gaps g $ TwoPane d r
  where
    s = (bspace c)
    d = (bdelta c)
    r = (bratio c)
    gapsize = (bgaps c)
    g = [(U, gapsize), (D, gapsize), (R, gapsize), (L, gapsize)]

data FullConf = FullConf
  { fgaps :: Int
  }

defaultFullConf :: FullConf
defaultFullConf = FullConf
  { fgaps = _gaps
  }

fullLayout c = gaps g $ Full
  where
    gapsize = (fgaps c)
    g = [(U, gapsize), (D, gapsize), (R, gapsize), (L, gapsize)]

data IMConf = IMConf
  { ilists      :: Property
  , ilist_ratio :: Rational
  , ispace      :: Integer
  , igaps       :: Int
  }

defaultIMConf :: IMConf
defaultIMConf = IMConf
  { ilists = (Role "buddy_list")
  , ilist_ratio = 1/5
  , ispace = _space
  , igaps = _gaps
  }
-- withIM (1/5) (Role "buddy_list") (Grid)
--imLayout :: (LayoutClass l a) => IMConf -> ModifiedLayout Spacing (ModifiedLayout AddRoster Grid) a
imLayout c = _spacing s $ withIM lr l (gaps g $ Grid)
  where
    s = (ispace c)
    lr = (ilist_ratio c)
    l = (ilists c)
    gapsize = (igaps c)
    g = [(U, gapsize), (D, gapsize), (R, gapsize), (L, gapsize)]
