
module Ob.Tests.Property (
  tests
  ) where

import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Ob as Ob

tests :: Spec
tests =
  describe "tail" $
    modifyMaxSuccess (const 1000) $
      it "inverts feis" $
        property $ \x -> Ob.tail (Ob.feis x) == x
