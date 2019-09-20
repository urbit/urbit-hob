{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import qualified Urbit.Ob as Ob

instance NFData Ob.Patp

patpGroup :: Benchmark
patpGroup = bgroup "patp" [
    bench "~zod" $
      nf Ob.patp 0x0000

  , bench "~marzod" $
      nf Ob.patp 0x0100

  , bench "~dapnep-ronmyl" $
      nf Ob.patp 0x00010000

  , bench "~fipfes-fipfes-dostec-risfen" $
      nf Ob.patp 0xFFFFFFFFFFFFFFFF

  , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
      nf Ob.patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  ]

fromPatpGroup :: Benchmark
fromPatpGroup = bgroup "fromPatp" [
   bench "~zod" $
     whnf Ob.fromPatp (Ob.patp 0x0000)

 , bench "~marzod" $
     whnf Ob.fromPatp (Ob.patp 0x0100)

 , bench "~dapnep-ronmyl" $
     whnf Ob.fromPatp (Ob.patp 0x00010000)

 , bench "~fipfes-fipfes-dostec-risfen" $
     whnf Ob.fromPatp (Ob.patp 0xFFFFFFFFFFFFFFFF)

 , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
     whnf Ob.fromPatp (Ob.patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
 ]

renderGroup :: Benchmark
renderGroup = bgroup "renderPatp" [
   bench "~zod" $
     nf Ob.renderPatp (Ob.patp 0x0000)

 , bench "~marzod" $
     nf Ob.renderPatp (Ob.patp 0x0100)

 , bench "~dapnep-ronmyl" $
     nf Ob.renderPatp (Ob.patp 0x00010000)

 , bench "~fipfes-fipfes-dostec-risfen" $
     nf Ob.renderPatp (Ob.patp 0xFFFFFFFFFFFFFFFF)

 , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
     nf Ob.renderPatp (Ob.patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
 ]

parseGroup :: Benchmark
parseGroup = bgroup "parsePatp" [
   bench "~zod" $
     nf Ob.parsePatp "~zod"

 , bench "~marzod" $
     nf Ob.parsePatp "~marzod"

 , bench "~dapnep-ronmyl" $
     nf Ob.parsePatp "~dapnep-ronmyl"

 , bench "~fipfes-fipfes-dostec-risfen" $
     nf Ob.parsePatp "~fipfes-fipfes-dostec-risfen"

 , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
     nf Ob.parsePatp "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"
 ]

main :: IO ()
main = defaultMain [
    patpGroup
  , fromPatpGroup
  , renderGroup
  , parseGroup
  ]
