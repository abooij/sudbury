{-|
Module      : Graphics.Sudbury.Argument
Description : Various type tricks to play with wayland arguments as types and values
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Sudbury.Argument where

import Data.Singletons.TH

data ArgumentType = IntWAT
                  | UIntWAT
                  | FixedWAT
                  | StringWAT
                  | ObjectWAT
                  | NewIdWAT
                  | ArrayWAT
                  | FdWAT

$(genSingletons [''ArgumentType])

data ArgTypeBox = forall t. ArgTypeBox (SArgumentType t)
