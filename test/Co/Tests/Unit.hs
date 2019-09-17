{-# LANGUAGE OverloadedStrings #-}

module Co.Tests.Unit (
  tests
  ) where

import Data.Word (Word32)
import Test.Hspec
import qualified Urbit.Ob.Co as Co

tests :: Spec
tests =
  describe "render" $ do
    it "matches 32-bit reference values" $ do
      Co.render (Co.patp 0) `shouldBe` "~zod"
      Co.render (Co.patp 255) `shouldBe` "~fes"
      Co.render (Co.patp 256) `shouldBe` "~marzod"
      Co.render (Co.patp 65535) `shouldBe` "~fipfes"
      Co.render (Co.patp 65536) `shouldBe` "~dapnep-ronmyl"
      Co.render (Co.patp 15663360) `shouldBe` "~nidsut-tomdun"
      Co.render (Co.patp 0xFFFFFFFF) `shouldBe` "~dostec-risfen"

    it "matches 64-bit reference values" $ do
      let big_64_01 = 0x0000000100000000
      Co.render (Co.patp big_64_01) `shouldBe` "~doznec-dozzod-dozzod"

      let big_64_02 = 0xFFFFFFFFFFFFFFFF
      Co.render (Co.patp big_64_02) `shouldBe` "~fipfes-fipfes-dostec-risfen"

    it "matches 128-bit reference values" $ do
      let big_128_01  = 0x00000000000000010000000000000000
          patp_128_01 = "~doznec--fipfes-fipfes-fipfes-fipfes"
      Co.render (Co.patp big_128_01) `shouldBe` patp_128_01

      let big_128_02  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          patp_128_02 =
            "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"

      Co.render (Co.patp big_128_02) `shouldBe` patp_128_02

