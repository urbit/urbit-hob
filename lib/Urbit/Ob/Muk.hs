
-- |
-- Module: Urbit.Ob.Muk
-- Copyright: (c) 2019 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.io>
-- Stability: unstable
-- Portability: ghc
--
-- A specific murmur3 variant.
--
-- Analogous to +muk in hoon.hoon.

module Urbit.Ob.Muk (
    muk
  ) where

import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Word (Word32)
import qualified Data.Hash.Murmur as M

-- | A specific murmur3 variant.
muk :: Word32 -> Word32 -> Word32
muk syd key = M.murmur3 syd kee where
  kee = chr lo `B8.cons` chr hi `B8.cons` mempty
  lo  = fromIntegral (key .&. 0xFF)
  hi  = fromIntegral (key .&. 0xFF00 `div` 0x0100)

