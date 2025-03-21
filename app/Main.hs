{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Exception (assert)
import Control.Monad (filterM, foldM, forM_, replicateM, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Array (Array)
import Data.Array.Base (newArray, readArray, writeArray)
import Data.Array.IArray qualified as IA
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (find, for_)
import Data.Heap qualified as Heap
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

main :: IO ()
main = pure ()

{-- 入出力 --}

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt

getInt :: IO Int
getInt = readInt <$> BS.getLine

getInts :: IO [Int]
getInts = map readInt . BS.words <$> BS.getLine

getTuple :: IO (Int, Int)
getTuple = do
  [a, b] <- getInts
  return (a, b)

getTuple3 :: IO (Int, Int, Int)
getTuple3 = do
  [a, b, c] <- getInts
  return (a, b, c)

getCharGrid :: ((Int, Int), (Int, Int)) -> IO (UArray (Int, Int) Char)
getCharGrid b@((s, _), (h, _)) = do
  xs <- replicateM (h + 1 - s) BS.getLine
  return $ IA.listArray @UArray b $ BS.unpack $ BS.concat xs

{-- mod --}

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

{-- 数論 --}

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

{-- 組合せ --}

factorialsAndInverses :: Int -> (V.Vector IntMod, V.Vector IntMod)
factorialsAndInverses n = (fact, invFact)
  where
    fact = V.scanl' (*) 1 $ V.generate n (IntMod . (+ 1))
    invFact = V.postscanr' (*) (recip (fact V.! n)) (V.generate n (IntMod . (+ 1)))

combination :: V.Vector IntMod -> V.Vector IntMod -> Int -> Int -> IntMod
combination fact invFact n k
  | k < 0 || k > n = 0
  | otherwise = fact V.! n * (invFact V.! k) * (invFact V.! (n - k))

{-- 探索 --}

bfs :: (IA.Ix v) => (v -> [v]) -> Int -> (v, v) -> [(v, Int)] -> UArray v Int
bfs nextStates initial bounds start = runSTUArray $ do
  dist <- newArray bounds initial
  for_ start $ uncurry (writeArray dist)

  let loop Seq.Empty = return ()
      loop (v Seq.:<| queue) = do
        d <- readArray dist v
        us <- filterM (fmap (== initial) . readArray dist) (nextStates v)
        queue' <- foldM (\q u -> do writeArray dist u (d + 1); return $ q Seq.|> u) queue us
        loop queue'

  loop $ Seq.fromList $ map fst start
  return dist

dijkstra :: (IA.Ix v) => (v -> [(v, Int)]) -> Int -> (v, v) -> [(v, Int)] -> UArray v Int
dijkstra nextStates initial bounds start = runSTUArray $ do
  dist <- newArray bounds initial
  for_ start $ uncurry (writeArray dist)

  let loop queue
        | Heap.null queue = return ()
        | otherwise = do
            let ((d, v), queue') = fromJust $ Heap.viewMin queue
            distV <- readArray dist v
            when (d > distV) $ return ()
            let us = nextStates v
            queue'' <-
              foldM
                ( \q (u, w) -> do
                    distU <- readArray dist u
                    let distU' = distV + w
                    if distU > distU'
                      then do
                        writeArray dist u distU'
                        return $ Heap.insert (distU', u) q
                      else return q
                )
                queue'
                us
            loop queue''

  loop $ Heap.fromList $ map (\(x, y) -> (y, x)) start
  return dist

{-- グラフ --}

constructGraph :: (IA.Ix v) => (v, v) -> [(v, e)] -> Array v [e]
constructGraph = IA.accumArray (flip (:)) []

{-- 配列 --}

findArrayIndex :: (IA.IArray a e, IA.Ix i) => (e -> Bool) -> a i e -> Maybe i
findArrayIndex f as = fst <$> find (f . snd) (IA.assocs as)
{-# INLINE findArrayIndex #-}

{-- リングバッファ --}

data Buffer s a = Buffer
  { bufferVars :: !(VUM.MVector s Int),
    internalBuffer :: !(VUM.MVector s a),
    internalBufferSize :: !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

newBuffer :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBuffer n = Buffer <$> VUM.replicate 2 0 <*> VUM.unsafeNew n <*> pure n

popFrontBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFrontBuf Buffer {bufferVars, internalBuffer, internalBufferSize} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f /= b
    then do
      VUM.unsafeWrite bufferVars _bufferFrontPos ((f + 1) `mod` internalBufferSize)
      pure <$> VUM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE popFrontBuf #-}

popBackBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popBackBuf Buffer {bufferVars, internalBuffer, internalBufferSize} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  let b' = (b - 1) `mod` internalBufferSize
  if f /= b
    then do
      VUM.unsafeWrite bufferVars _bufferBackPos b'
      pure <$> VUM.unsafeRead internalBuffer b'
    else return Nothing
{-# INLINE popBackBuf #-}

pushFrontBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFrontBuf Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  let f' = (f - 1) `mod` internalBufferSize
  VUM.unsafeWrite bufferVars _bufferFrontPos f'
  assert (f' /= b) $ do
    VUM.unsafeWrite internalBuffer f' x
{-# INLINE pushFrontBuf #-}

pushBackBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBackBuf Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  let b' = (b + 1) `mod` internalBufferSize
  VUM.unsafeWrite bufferVars _bufferBackPos b'
  assert (b' /= f) $ do
    VUM.unsafeWrite internalBuffer b x
{-# INLINE pushBackBuf #-}
