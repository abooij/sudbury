{-|
Module      : Graphics.Sudbury.Object
Description : Wayland object handling
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

This module can generate Haskell types which will represent Wayland objects,
and can marshal/demarshal values into/out of them.
-}
{-# LANGUAGE Trustworthy #-}
-- Simply importing Language.Haskell.TH is already considered unsafe.
-- The fact that we only generate some data in a pure way does not matter here.
module Graphics.Sudbury.Object where

import Language.Haskell.TH

import Graphics.Sudbury.Protocol.Types

generatePlainType :: String -> [String] -> Dec
generatePlainType name constrs =
  DataD [] (mkName name) [] (map (flip NormalC [] . mkName) constrs) []

protocolTypeData :: WLProtocol -> (String , [String])
protocolTypeData prot = (protocolName prot , map interfaceName (protocolInterfaces prot))
