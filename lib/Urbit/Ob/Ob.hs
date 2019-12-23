{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Urbit.Ob.Ob
-- Copyright: (c) 2019 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.io>
-- Stability: unstable
-- Portability: ghc
--
-- Integer obfuscation functions.
--
-- Analogous to the +ob arm in hoon.hoon.

module Urbit.Ob.Ob (
    fein
  , fynd
  , feis
  , tail
  , fe
  , fen
  , capF
  , capFe
  , capFen
  ) where

import Data.Bits
import Data.Word (Word32)
import Prelude hiding (tail)
import Urbit.Ob.Muk (muk)

-- | Conceal structure v3.
fein :: (Integral a, Bits a) => a -> a
fein = loop where
  loop !pyn =
    let lo  = pyn .&. 0xFFFFFFFF
        hi  = pyn .&. 0xFFFFFFFF00000000
        p32 = fromIntegral pyn :: Word32
    in  if   pyn >= 0x10000 && pyn <= 0xFFFFFFFF
        then 0x10000 + fromIntegral (feis (p32 - 0x10000))
        else if   pyn >= 0x100000000 && pyn <= 0xFFFFFFFFFFFFFFFF
             then hi .|. loop lo
             else pyn

-- | Restore structure v3.
fynd :: (Integral a, Bits a) => a -> a
fynd = loop where
  loop !cry =
    let lo  = cry .&. 0xFFFFFFFF
        hi  = cry .&. 0xFFFFFFFF00000000
        c32 = fromIntegral cry :: Word32
    in  if   cry >= 0x10000 && cry <= 0xFFFFFFFF
        then 0x10000 + fromIntegral (tail (c32 - 0x10000))
        else if   cry >= 0x100000000 && cry <= 0xFFFFFFFFFFFFFFFF
             then hi .|. loop lo
             else cry

-- | Generalised Feistel cipher.
--
--   See: Black and Rogaway (2002), "Ciphers with arbitrary finite domains."
--
--   Note that this has been adjusted from the reference paper in order to
--   support some legacy behaviour.
feis :: Word32 -> Word32
feis = capFe 4 0xFFFF 0x10000 0xFFFFFFFF capF

-- | Reverse 'feis'.
--
--   See: Black and Rogaway (2002), "Ciphers with arbitrary finite domains."
--
--   Note that this has been adjusted from the reference paper in order to
--   support some legacy behaviour.
tail :: Word32 -> Word32
tail = capFen 4 0xFFFF 0x10000 0xFFFFFFFF capF

-- | A PRF for j in [0, .., 3]
capF :: Int -> Word32 -> Word32
capF j key = fromIntegral (muk seed key) where
  seed = raku !! fromIntegral j
  raku = [
      0xb76d5eed
    , 0xee281300
    , 0x85bcae01
    , 0x4b387af7
    ]

-- | 'Fe' in B&R (2002).
capFe
  :: Int
  -> Word32
  -> Word32
  -> Word32
  -> (Int -> Word32 -> Word32)
  -> Word32
  -> Word32
capFe r a b k f m
    | c < k     = c
    | otherwise = fe r a b f c
  where
    c = fe r a b f m

-- | 'fe' in B&R (2002).
fe
  :: Int
  -> Word32
  -> Word32
  -> (Int -> Word32 -> Word32)
  -> Word32
  -> Word32
fe r a b f m = loop 1 capL capR where
  capL = m `mod` a
  capR = m `div` a
  loop j !ell !arr
    | j > r =
        if   odd r || arr == a
        then a * arr + ell
        else a * ell + arr
    | otherwise =
        let eff   = f (pred j) arr
            -- NB (jtobin):
            --
            -- note that the "extra" modulo operators here are not redundant as
            -- the addition of ell and eff can (silently) overflow Word32.
            -- modulo p does not distribute over addition, but it does
            -- "distribute modulo p," so this ensures we stay sufficiently
            -- small.
            tmp  = if   odd j
                   then (ell `mod` a + eff `mod` a) `mod` a
                   else (ell `mod` b + eff `mod` b) `mod` b

        in  loop (succ j) arr tmp

-- | 'Fen' in B&R (2002).
capFen
  :: Int
  -> Word32
  -> Word32
  -> Word32
  -> (Int -> Word32 -> Word32)
  -> Word32
  -> Word32
capFen r a b k f m
    | c <= k    = c
    | otherwise = fen r a b f c
  where
    c = fen r a b f m

-- | 'fen' in B&R (2002).
fen
  :: Int
  -> Word32
  -> Word32
  -> (Int -> Word32 -> Word32)
  -> Word32
  -> Word32
fen r a b f m = loop r capL capR where
  ahh =
    if   odd r
    then m `div` a
    else m `mod` a

  ale =
    if   odd r
    then m `mod` a
    else m `div` a

  capL =
    if   ale == a
    then ahh
    else ale

  capR =
    if   ale == a
    then ale
    else ahh

  loop j !ell !arr
    | j < 1     = a * arr + ell
    | otherwise =
        let eff = f (pred j) ell
            -- NB (jtobin):
            --
            -- Slight deviation from B&R (2002) here to prevent negative
            -- values.  We add 'a' or 'b' to arr as appropriate and reduce
            -- 'eff' modulo the same number before performing subtraction.
            --
            tmp = if   odd j
                  then (arr + a - (eff `mod` a)) `mod` a
                  else (arr + b - (eff `mod` b)) `mod` b
        in  loop (pred j) tmp ell


