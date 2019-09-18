
module Co.Tests.Property (
  tests
  ) where

import qualified Data.Text as T
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Co as Co

nats :: Gen Natural
nats = fmap fromIntegral (arbitrary :: Gen Word32)

patps :: Gen Co.Patp
patps = fmap Co.patp nats

patpStrings :: Gen T.Text
patpStrings = fmap Co.render patps

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

  describe "render" $
    modifyMaxSuccess (const 1000) $
      it "inverts parse" $
        forAll patpStrings $ \x ->
          case Co.parse x of
            Left _  -> False
            Right p -> Co.render p == x

