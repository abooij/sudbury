module Wire where

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Graphics.Sudbury.WirePackages

import Arbitrary ()

tests :: Test
tests = testGroup "Wire protocol transformations"
  [ testProperty "wire packages identity" prop_package_id
  ]

-- | Test if packing and then parsing a 'WirePackage' yields the original package.
prop_package_id :: WirePackage -> Property
prop_package_id pkg = Just True === AB.compareResults (AB.Done B.empty pkg)
  (AB.parse parseWirePackage . toStrict . toLazyByteString . wirePack $ pkg)
