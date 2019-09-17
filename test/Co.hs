module Main where

import qualified Co.Tests.Property as P
import qualified Co.Tests.Unit as U
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    context "property tests" P.tests
    context "unit tests" U.tests

