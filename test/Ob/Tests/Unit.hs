
module Ob.Tests.Unit (
  tests
  ) where

import Test.Hspec
import qualified Urbit.Ob.Ob as Ob

tests :: Spec
tests =
  describe "tail . feis" $ do

    context "when applied to 2052065766" $
      it "should be the identity function" $
        Ob.tail (Ob.feis 2052065766) `shouldBe` 2052065766

