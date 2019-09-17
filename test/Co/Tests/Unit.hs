{-# LANGUAGE OverloadedStrings #-}

module Co.Tests.Unit (
  tests
  ) where

import Data.Word (Word32)
import Test.Hspec
import qualified Urbit.Ob.Co as Co

tests :: Spec
tests =
  describe "render" $
    it "matches 32-bit reference values" $ do
      Co.render (Co.patp 0) `shouldBe` "~zod"
      Co.render (Co.patp 255) `shouldBe` "~fes"
      Co.render (Co.patp 256) `shouldBe` "~marzod"
      Co.render (Co.patp 65535) `shouldBe` "~fipfes"
      Co.render (Co.patp 65536) `shouldBe` "~dapnep-ronmyl"
      Co.render (Co.patp 15663360) `shouldBe` "~nidsut-tomdun"
      Co.render (Co.patp 0xFFFFFFFF) `shouldBe` "~dostec-risfen"

