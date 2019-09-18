
module Data.Serialize.Extended (
    roll
  , unroll
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Numeric.Natural (Natural)

-- | Simple little-endian ByteString encoding for Naturals.
unroll :: Natural -> BS.ByteString
unroll = BS.pack . unfoldr step where
  step 0 = Nothing
  step i = Just (fromIntegral i, i `shiftR` 8)

-- | Simple little-endian ByteString decoding for Naturals.
roll :: BS.ByteString -> Natural
roll = foldr unstep 0 . BS.unpack where
  unstep b a = a `shiftL` 8 .|. fromIntegral b

