
module Title.Tests.Property (
  tests
  ) where

import Data.Word (Word8, Word16, Word32, Word64)
import Test.Hspec
import Test.QuickCheck
import Urbit.Ob.Co (Patp)
import qualified Urbit.Ob.Co as Co
import qualified Urbit.Ob.Title as Title

galaxies :: Gen Patp
galaxies = fmap (Co.patp . fromIntegral) (arbitrary :: Gen Word8)

stars :: Gen Patp
stars = do
  star <- arbitrary `suchThat` (> (0xFF :: Word16))
  return (Co.patp (fromIntegral star))

planets :: Gen Patp
planets = do
  planet <- arbitrary `suchThat` (> (0xFFFF :: Word32))
  return (Co.patp (fromIntegral planet))

moons :: Gen Patp
moons = do
  moon <- arbitrary `suchThat` (> (0xFFFFFFFF :: Word64))
  return (Co.patp (fromIntegral moon))

tests :: Spec
tests =
  describe "clan" $ do
    it "identifies galaxies correctly" $
      forAll galaxies $ \x -> Title.clan x == Title.Galaxy

    it "identifies stars correctly" $
      forAll stars $ \x -> Title.clan x == Title.Star

    it "identifies planets correctly" $
      forAll planets $ \x -> Title.clan x == Title.Planet

    it "identifies moons correctly" $
      forAll moons $ \x -> Title.clan x == Title.Moon

