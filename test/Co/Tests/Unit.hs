{-# LANGUAGE OverloadedStrings #-}

module Co.Tests.Unit (
  tests
  ) where

import Data.Word (Word32)
import Test.Hspec
import qualified Urbit.Ob.Co as Co

tests :: Spec
tests = do
  describe "renderPatp" $ do
    it "matches 32-bit reference values" $ do
      Co.renderPatp (Co.patp 0) `shouldBe` "~zod"
      Co.renderPatp (Co.patp 255) `shouldBe` "~fes"
      Co.renderPatp (Co.patp 256) `shouldBe` "~marzod"
      Co.renderPatp (Co.patp 65535) `shouldBe` "~fipfes"
      Co.renderPatp (Co.patp 65536) `shouldBe` "~dapnep-ronmyl"
      Co.renderPatp (Co.patp 15663360) `shouldBe` "~nidsut-tomdun"
      Co.renderPatp (Co.patp 0xFFFFFFFF) `shouldBe` "~dostec-risfen"

    it "matches 64-bit reference values" $ do
      let big_64_01 = 0x0000000100000000
      Co.renderPatp (Co.patp big_64_01) `shouldBe` "~doznec-dozzod-dozzod"

      let big_64_02 = 0xFFFFFFFFFFFFFFFF
      Co.renderPatp (Co.patp big_64_02) `shouldBe` "~fipfes-fipfes-dostec-risfen"

    it "matches 128-bit reference values" $ do
      let big_128_01  = 0x00000000000000010000000000000000
          patp_128_01 = "~doznec--dozzod-dozzod-dozzod-dozzod"
      Co.renderPatp (Co.patp big_128_01) `shouldBe` patp_128_01

      let big_128_02  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          patp_128_02 =
            "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"

      Co.renderPatp (Co.patp big_128_02) `shouldBe` patp_128_02

  describe "renderPatq" $ do
    it "matches 32-bit reference values" $ do
      Co.renderPatq (Co.patq 0) `shouldBe` "~zod"
      Co.renderPatq (Co.patq 255) `shouldBe` "~fes"
      Co.renderPatq (Co.patq 256) `shouldBe` "~marzod"
      Co.renderPatq (Co.patq 65535) `shouldBe` "~fipfes"
      Co.renderPatq (Co.patq 65536) `shouldBe` "~nec-dozzod"
      Co.renderPatq (Co.patq 15663360) `shouldBe` "~mun-marzod"
      Co.renderPatq (Co.patq 0xFFFFFFFF) `shouldBe` "~fipfes-fipfes"

    it "matches 64-bit reference values" $ do
      let big_64_01 = 0x0000000100000000
      Co.renderPatq (Co.patq big_64_01) `shouldBe` "~nec-dozzod-dozzod"

      let big_64_02 = 0xFFFFFFFFFFFFFFFF
          patq_64_02 = "~fipfes-fipfes-fipfes-fipfes"
      Co.renderPatq (Co.patq big_64_02) `shouldBe` patq_64_02

    it "matches 128-bit reference values" $ do
      let big_128_01  = 0x00000000000000010000000000000000
          patq_128_01 = "~nec-dozzod-dozzod-dozzod-dozzod"
      Co.renderPatq (Co.patq big_128_01) `shouldBe` patq_128_01

      let big_128_02  = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          patq_128_02 =
            "~fipfes-fipfes-fipfes-fipfes-fipfes-fipfes-fipfes-fipfes"

      Co.renderPatq (Co.patq big_128_02) `shouldBe` patq_128_02

