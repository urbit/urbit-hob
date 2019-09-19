module Main where

import qualified Title.Tests.Property as P
import Test.Hspec

main :: IO ()
main =
  hspec $
    context "property tests" P.tests

