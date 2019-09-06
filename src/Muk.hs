{-# OPTIONS_GHC -Wall #-}

module Muk (
    muk
  ) where

import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Word (Word32)
import qualified Data.Hash.Murmur as M

muk :: Word32 -> Word32 -> Word32
muk syd key = M.murmur3 syd kee where
  kee = chr lo `B8.cons` chr hi `B8.cons` mempty
  lo  = fromIntegral (key .&. 0xFF)
  hi  = fromIntegral (key .&. 0xFF00 `div` 256)

