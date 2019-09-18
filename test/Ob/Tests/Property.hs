
module Ob.Tests.Property (
  tests
  ) where

import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Urbit.Ob.Ob as Ob

planets :: Gen Word32
planets = arbitrary `suchThat` (> 0xFFFF)

word64 :: Gen Word64
word64 = arbitrary

nat :: Gen Natural
nat = do
  a <- fmap fromIntegral word64
  b <- fmap fromIntegral word64
  return (a * b)

tests :: Spec
tests = do
  describe "fynd" $
    modifyMaxSuccess (const 1000) $
      it "inverts fein" $
        forAll nat $ \x ->
          Ob.fynd (Ob.fein x) == x

  describe "fein" $
    modifyMaxSuccess (const 1000) $
      it "inverts fynd" $
        forAll nat $ \x ->
          Ob.fein (Ob.fynd x) == x

  describe "feis" $
    modifyMaxSuccess (const 1000) $
      it "inverts tail" $
        forAll planets $ \planet -> property $
          Ob.feis (Ob.tail planet) == planet

  describe "tail" $
    modifyMaxSuccess (const 1000) $
      it "inverts feis" $
        forAll planets $ \planet -> property $
          Ob.tail (Ob.feis planet) == planet

