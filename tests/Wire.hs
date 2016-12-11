module Wire where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Store as S

import Graphics.Sudbury.WirePackages

import Arbitrary ()

tests :: Test
tests = testGroup "Wire protocol transformations"
  [ testProperty "wire packages identity" prop_package_id
  ]

-- | Test if packing and then parsing a 'WirePackage' yields the original package.
prop_package_id :: WirePackage -> Property
prop_package_id = (===) <*> (S.decodeEx . S.encode)
