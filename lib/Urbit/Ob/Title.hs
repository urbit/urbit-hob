{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Urbit.Ob.Title
-- Copyright: (c) 2019 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.io>
-- Stability: unstable
-- Portability: ghc
--
-- Functions for determining ship class and parentage.
--
-- Analogous to the +title arm of zuse.hoon.

module Urbit.Ob.Title (
    Class(..)
  , clan
  , sein
  ) where

import Urbit.Ob.Co (Patp)
import qualified Urbit.Ob.Co as Co (patp, fromPatp)

-- | Ship class.
data Class =
    -- | 8-bit atom
    --
    Galaxy
    -- | 16-bit atom
    --
  | Star
    -- | 32-bit atom
    --
  | Planet
    -- | 64-bit atom
    --
  | Moon
    -- | 128-bit atom
    --
  | Comet
  deriving (Eq, Show)

-- | Determine ship class.
--
--   >>> let fes = patp 255
--   >>> let fipfes = patp 256
--   >>> let dostec = patp 0xFFFFFFFF
--   >>> let risfen = patp 0xFFFFFFFFFFFFFFFF
--   >>> let fipfesfipfes = patp 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
--   >>> clan fes
--   Galaxy
--   >>> clan fipfes
--   Star
--   >>> clan dostec
--   Planet
--   >>> clan risfen
--   Moon
--   >>> clan fipfesfipfes
--   Comet
--
clan :: Patp -> Class
clan ship
    | wid <= 1  = Galaxy
    | wid == 2  = Star
    | wid <= 4  = Planet
    | wid <= 8  = Moon
    | otherwise = Comet
  where
    wid = met 3 (Co.fromPatp ship)

-- | Determine parent.
--
--   A ship's parent signs for it on the network.  'sein' establishes the
--   so-called /autoboss/ of a ship (which can escape to another sponsor).
--
--   Note that galaxies sign for themselves, and stars sign for comets.
--
--   >>> sein fes
--   ~fes
--   >>> sein fipfes
--   ~fes
--   >>> sein dostec
--   ~fipfes
--   >>> sein risfen
--   ~dostec-risfen
--   >>> sein fipfesfipfes
--   ~fipfes
--
sein :: Patp -> Patp
sein ship = Co.patp $ case clan ship of
    Galaxy -> nat
    Star   -> end 3 1 nat
    Planet -> end 4 1 nat
    Moon   -> end 5 1 nat
    Comet  -> end 4 1 nat
  where
    nat = Co.fromPatp ship

met :: Integral a => a -> a -> a
met = loop 0 where
  loop !acc a !b
    | b == 0    = acc
    | otherwise = loop (succ acc) a (rsh a 1 b)

rsh :: Integral a => a -> a -> a -> a
rsh a b c = c `div` 2 ^ (2 ^ a * b)

end :: Integral a => a -> a -> a -> a
end a b c = c `mod` 2 ^ (2 ^ a * b)

