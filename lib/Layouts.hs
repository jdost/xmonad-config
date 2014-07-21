module Layouts (
    NormalConf (..)
  , defaultNormalConf
  , normalLayout

  , BrowserConf (..)
  , defaultBrowserConf
  , browserLayout

  , IMConf (..)
  , defaultIMConf
  , imLayout
  , layoutAliases
  ) where

import XMonad.Core (LayoutClass)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Spacing

layoutAliases :: [(String, String)]
layoutAliases =
  [ ("Hinted Spacing 4 ResizableTall", " RT")
  , ("Hinted Mirror Spacing 4 ResizableTall", "MRT")
  , ("Hinted Full", " F ")
  , ("Hinted Spacing 4 TwoPane", " 2P")
  , ("Hinted Spacing 4 IM Grid", " IM")
  ]

_space :: Int
_space = 4

data NormalConf = NormalConf
  { nmaster  :: Int
  , ndelta   :: Rational
  , nratio   :: Rational
  , nspace   :: Int
  }

defaultNormalConf :: NormalConf
defaultNormalConf = NormalConf
  { nmaster = 1
  , ndelta = 1/2
  , nratio = 3/100
  , nspace = _space
  }

--normalLayout :: (LayoutClass l a) => NormalConf -> ModifiedLayout Spacing ResizableTall a
normalLayout c = spacing s $ ResizableTall m r d []
  where
    s = (nspace c)
    m = (nmaster c)
    d = (ndelta c)
    r = (nratio c)

data BrowserConf = BrowserConf
  { bdelta :: Rational
  , bratio :: Rational
  , bspace :: Int
  }

defaultBrowserConf :: BrowserConf
defaultBrowserConf = BrowserConf
  { bdelta = 3/100
  , bratio = 1/2
  , bspace = _space
  }
-- TwoPane delta ratio
--browserLayout :: (LayoutClass l a) => BrowserConf -> ModifiedLayout Spacing TwoPane a
browserLayout c = spacing s $ TwoPane d r
  where
    s = (bspace c)
    d = (bdelta c)
    r = (bratio c)

data IMConf = IMConf
  { ilists :: Property
  , ilist_ratio :: Rational
  , ispace :: Int
  }

defaultIMConf :: IMConf
defaultIMConf = IMConf
  { ilists = (Role "buddy_list")
  , ilist_ratio = 1/5
  , ispace = _space
  }
-- withIM (1/5) (Role "buddy_list") (Grid)
--imLayout :: (LayoutClass l a) => IMConf -> ModifiedLayout Spacing (ModifiedLayout AddRoster Grid) a
imLayout c = spacing s $ withIM lr l (Grid)
  where
    s = (ispace c)
    lr = (ilist_ratio c)
    l = (ilists c)
