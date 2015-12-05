module Main (main) where

import Test.Framework (defaultMain)

import qualified Wire

main :: IO ()
main = defaultMain
  [ Wire.tests
  ]
