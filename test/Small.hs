module Main where

import Control.Monad (unless)
import Data.List (nub, foldl')
import Prelude hiding (tail)
import System.Exit (exitSuccess, exitFailure)
import qualified Urbit.Ob.Ob as Ob

a, b, c :: Int
a = 2 ^ 2 - 1
b = 2 ^ 2
c = a * b

eff j m =
  let v0 = [5, 9, 2, 6, 4, 0, 8, 7, 1, 10, 3, 11]
      v1 = [2, 1, 0, 3, 10, 4, 9, 5, 7, 11, 6, 8]
      v2 = [10, 6, 7, 1, 0, 11, 3, 9, 5, 2, 8, 4]
      v3 = [11, 0, 3, 5, 9, 8, 6, 10, 4, 1, 2, 7]

  in  case j of
        0 -> v0 !! m
        1 -> v1 !! m
        2 -> v2 !! m
        _ -> v3 !! m

feis = Ob.capFe 4 a b c eff

tail = Ob.capFen 4 a b c eff

main :: IO ()
main = do
  let emm  = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
      perm = fmap feis emm
      inv  = fmap tail perm
      distincts = nub perm

  let distinctElems = length distincts == length perm
  unless distinctElems exitFailure

  let permuteSuccessful = foldl' (\acc x -> x `elem` emm && acc) True perm
  unless permuteSuccessful exitFailure

  let tailInvertsFeis = emm == inv
  unless tailInvertsFeis exitFailure

  exitSuccess

