{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ws (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ws"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Stack-based interpreter program"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
