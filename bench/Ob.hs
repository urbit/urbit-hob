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
renderGroup = bgroup "render" [
   bench "~zod" $
     nf Ob.render (Ob.patp 0x0000)

 , bench "~marzod" $
     nf Ob.render (Ob.patp 0x0100)

 , bench "~dapnep-ronmyl" $
     nf Ob.render (Ob.patp 0x00010000)

 , bench "~fipfes-fipfes-dostec-risfen" $
     nf Ob.render (Ob.patp 0xFFFFFFFFFFFFFFFF)

 , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
     nf Ob.render (Ob.patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
 ]

parseGroup :: Benchmark
parseGroup = bgroup "parse" [
   bench "~zod" $
     nf Ob.parse "~zod"

 , bench "~marzod" $
     nf Ob.parse "~marzod"

 , bench "~dapnep-ronmyl" $
     nf Ob.parse "~dapnep-ronmyl"

 , bench "~fipfes-fipfes-dostec-risfen" $
     nf Ob.parse "~fipfes-fipfes-dostec-risfen"

 , bench "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes" $
     nf Ob.parse "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"
 ]

main :: IO ()
main = defaultMain [
    patpGroup
  , fromPatpGroup
  , renderGroup
  , parseGroup
  ]
