module Main where

import qualified Ob.Tests.Small as S
import qualified Ob.Tests.Med as M
import qualified Ob.Tests.Property as P
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    context "small input space" S.tests
    context "medium input space" M.tests
    context "32-bit input space" P.tests
