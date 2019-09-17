
module Co.Tests.Property (
  tests
  ) where

import Numeric.Natural
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Co as Co

nats :: Gen Natural
nats = fmap fromIntegral (arbitrary `suchThat` (>= (0 :: Int)))

patps :: Gen Co.Patp
patps = fmap Co.patp nats

tests :: Spec
tests = do
  describe "fromPatp" $
    modifyMaxSuccess (const 1000) $
      it "inverts patp" $
        forAll nats $ \x -> Co.fromPatp (Co.patp x) == x

  describe "patp" $
    modifyMaxSuccess (const 1000) $
      it "inverts fromPatp" $
        forAll patps $ \x -> Co.patp (Co.fromPatp x) == x

