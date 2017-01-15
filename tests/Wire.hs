module Wire where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Store as S
import qualified Data.ByteString as B

import Graphics.Sudbury.WirePackages

import Arbitrary

tests :: Test
tests = testGroup "Wire protocol transformations"
  [ testProperty "wire packages identity" prop_package_id
  , testProperty "wire stream encoding identity" prop_package_stream_id
  , testProperty "wire stream decoding identity" prop_package_binary_id
  ]

-- | Test if packing and then parsing a 'WirePackage' yields the original package.
prop_package_id :: WirePackage -> Property
prop_package_id = (===) <*> (S.decodeEx . S.encode)

prop_package_stream_id :: WirePackageStream -> Property
prop_package_stream_id = (===) <*> (WirePackageStream . S.decodeEx . S.encode . unWirePackageStream)

prop_package_binary_id :: WirePackageBinary -> Property
prop_package_binary_id = (===) <*> (WirePackageBinary . S.encode . decode' . unWpBin)
  where
    decode' :: B.ByteString -> WirePackageStream
    decode' = S.decodeEx


