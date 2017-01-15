module Main (main) where

import Test.Framework (defaultMain)

import qualified Socket
import qualified Wire

main :: IO ()
main = defaultMain
  [ Socket.tests
  , Wire.tests
  ]
