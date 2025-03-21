{-# LANGUAGE ImportQualifiedPost #-}

module Combination (factorialsAndInverses, combination) where

import Data.Vector qualified as V
import IntMod (IntMod (IntMod))

factorialsAndInverses :: Int -> (V.Vector IntMod, V.Vector IntMod)
factorialsAndInverses n = (fact, invFact)
  where
    fact = V.scanl' (*) 1 $ V.generate n (IntMod . (+ 1))
    invFact = V.postscanr' (*) (recip (fact V.! n)) (V.generate n (IntMod . (+ 1)))

combination :: V.Vector IntMod -> V.Vector IntMod -> Int -> Int -> IntMod
combination fact invFact n k
  | k < 0 || k > n = 0
  | otherwise = fact V.! n * (invFact V.! k) * (invFact V.! (n - k))
