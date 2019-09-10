module Main where

import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Co as Co

main :: IO ()
main = hspec $
  describe "fromPatp" $
    modifyMaxSuccess (const 1000) $
      it "inverts patp" $
        property $ \(NonNegative x) -> Co.fromPatp (Co.patp x) == x

