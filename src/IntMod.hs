module IntMod (IntMod (IntMod), extGCD, invMod, modPow) where

import Data.Ratio (denominator, numerator)

modulus :: Int
modulus = 998244353

extGCD :: Int -> Int -> (Int, Int, Int)
extGCD a 0 = (a, 1, 0)
extGCD a b =
  let (g, x, y) = extGCD b (a `mod` b)
   in (g, y, x - (a `div` b) * y)

invMod :: Int -> Int
invMod a = case extGCD a modulus of
  (1, s, _) -> s `mod` modulus
  (-1, s, _) -> (-s) `mod` modulus
  _ -> error $ show a ++ " has no inverse modulo " ++ show modulus

newtype IntMod = IntMod Int deriving (Eq)

instance Show IntMod where
  show (IntMod n) = show n

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `rem` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `rem` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = undefined
  signum = undefined

instance Fractional IntMod where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  IntMod x / IntMod y = IntMod (x * invMod y `rem` modulus)

modPow :: IntMod -> Int -> IntMod
modPow _ 0 = 1
modPow a n =
  let x = modPow a (n `div` 2)
   in if even n then x * x else a * x * x
