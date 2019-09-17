
module Ob.Tests.Unit (
  tests
  ) where

import Data.Word (Word32)
import Test.Hspec
import qualified Urbit.Ob.Ob as Ob

tests :: Spec
tests = do
  describe "fein" $
    it "matches reference values" $ do
      Ob.fein 123456789 `shouldBe` (1897766331 :: Int)
      Ob.fein 15663360 `shouldBe` (1208402137 :: Int)

  describe "fynd" $
    it "matches reference values" $ do
      Ob.fynd 1208402137 `shouldBe` (15663360 :: Int)
      Ob.fynd 1897766331 `shouldBe` (123456789 :: Int)

  describe "feis" $
    it "matches reference values" $ do
      Ob.feis 123456789 `shouldBe` (2060458291 :: Word32)
      Ob.feis 15663360 `shouldBe` (1195593620 :: Word32)

  describe "tail" $
    it "matches reference values" $ do
      Ob.tail 123456789 `shouldBe` (1107963580 :: Word32)
      Ob.tail 1195593620 `shouldBe` (15663360 :: Word32)

