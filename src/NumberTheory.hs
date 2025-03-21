{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

module NumberTheory where

import Control.Monad (forM_, when)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

type PrimeTable = VU.Vector (Bool, Int)

sieve :: Int -> PrimeTable
sieve n = VU.create $ do
  vec <- VUM.generate (n + 1) (True,)
  VUM.write vec 0 (False, -1)
  VUM.write vec 1 (False, -1)
  forM_ [2 .. n] $ \i -> do
    (b, _) <- VUM.read vec i
    when b $ do
      forM_ [2 * i, 3 * i .. n] $ \j -> do
        (b', _) <- VUM.read vec j
        when b' $ VUM.write vec j (False, i)
  return vec

primes :: PrimeTable -> [Int]
primes t = [i | i <- [0 .. VU.length t - 1], fst (t VU.! i)]

fastFactorize :: PrimeTable -> Int -> [(Int, Int)]
fastFactorize t n = go n []
  where
    go 1 acc = reverse acc
    go m acc = let (_, p) = t VU.! m in go (m `div` p) (incCount p acc)
    incCount p [] = [(p, 1)]
    incCount p ((q, c) : xs)
      | p == q = (q, c + 1) : xs
      | otherwise = (p, 1) : (q, c) : xs
