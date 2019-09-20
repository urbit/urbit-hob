
module Co.Tests.Property (
  tests
  ) where

import qualified Data.Text as T
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Co as Co

nats :: Gen Natural
nats = fmap fromIntegral (arbitrary :: Gen Word64)

patps :: Gen Co.Patp
patps = fmap Co.patp nats

patqs :: Gen Co.Patq
patqs = fmap Co.patq nats

patpStrings :: Gen T.Text
patpStrings = fmap Co.renderPatp patps

patqStrings :: Gen T.Text
patqStrings = fmap Co.renderPatq patqs

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

  describe "renderPatp" $
    modifyMaxSuccess (const 1000) $
      it "inverts parsePatp" $
        forAll patpStrings $ \x ->
          case Co.parsePatp x of
            Left _  -> False
            Right p -> Co.renderPatp p == x

  describe "fromPatq" $
    modifyMaxSuccess (const 1000) $
      it "inverts patq" $
        forAll nats $ \x -> Co.fromPatq (Co.patq x) == x

  describe "patq" $
    modifyMaxSuccess (const 1000) $
      it "inverts fromPatq" $
        forAll patqs $ \x -> Co.patq (Co.fromPatq x) == x

  describe "renderPatq" $
    modifyMaxSuccess (const 1000) $
      it "inverts parsePatq" $
        forAll patqStrings $ \x ->
          case Co.parsePatq x of
            Left _  -> False
            Right p -> Co.renderPatq p == x

