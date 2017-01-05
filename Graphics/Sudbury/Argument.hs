{-|
Module      : Graphics.Sudbury.Argument
Description : Various type tricks to play with wayland arguments as types and values
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
{-# LANGUAGE Trustworthy #-}
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

instance Eq ArgTypeBox where
  ArgTypeBox SIntWAT    == ArgTypeBox SIntWAT = True
  ArgTypeBox SUIntWAT   == ArgTypeBox SUIntWAT = True
  ArgTypeBox SFixedWAT  == ArgTypeBox SFixedWAT = True
  ArgTypeBox SStringWAT == ArgTypeBox SStringWAT = True
  ArgTypeBox SObjectWAT == ArgTypeBox SObjectWAT = True
  ArgTypeBox SNewIdWAT  == ArgTypeBox SNewIdWAT = True
  ArgTypeBox SArrayWAT  == ArgTypeBox SArrayWAT = True
  ArgTypeBox SFdWAT     == ArgTypeBox SFdWAT = True
  _ == _ = False

instance Show ArgTypeBox where
  show (ArgTypeBox SIntWAT)    = "SIntWAT"
  show (ArgTypeBox SUIntWAT)   = "SUIntWAT"
  show (ArgTypeBox SFixedWAT)  = "SFixedWAT"
  show (ArgTypeBox SStringWAT) = "SStringWAT"
  show (ArgTypeBox SObjectWAT) = "SObjectWAT"
  show (ArgTypeBox SNewIdWAT)  = "SNewIdWAT"
  show (ArgTypeBox SArrayWAT)  = "SArrayWAT"
  show (ArgTypeBox SFdWAT)     = "SFdWAT"
