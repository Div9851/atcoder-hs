{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Comonad.Representable.Store (ComonadStore (pos))
import Control.Exception (assert)
import Control.Monad (filterM, foldM, foldM_, forM, forM_, replicateM, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Array (Array)
import Data.Array.Base (newArray, readArray, writeArray)
import Data.Array.IArray qualified as IA
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.&.), (.<<.), (.>>.), (.^.))
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (find, foldl', for_)
import Data.Heap qualified as Heap
import Data.List qualified as L
import Data.Maybe (fromJust, isJust)
import Data.Ord (Down (..), comparing)
import Data.Ratio (denominator, numerator)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector.Algorithms.Search (binarySearchL, binarySearchR)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace (traceShow)

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

{-- 整数論 --}

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
  let x = modPow a (n .>>. 1)
   in if even n then x * x else a * x * x

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

{-- グラフ --}

constructGraph :: (IA.Ix v) => (v, v) -> [(v, e)] -> Array v [e]
constructGraph = IA.accumArray (flip (:)) []

around4 :: UArray (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
around4 grid (x, y) = [pos | pos <- [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)], check pos]
  where
    check pos = IA.inRange (IA.bounds grid) pos && grid IA.! pos /= '#'

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

  let loop queue =
        case Heap.viewMin queue of
          Nothing -> return ()
          Just ((d, v), queue') -> do
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

{-- リスト/配列 --}

powerSet :: [a] -> [[a]]
powerSet = filterM (const [False, True])

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) =
  map (x :) (combinations (k - 1) xs)
    ++ combinations k xs

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x : xs) =
  [(x : ys) : rest | (ys : rest) <- partitions xs]
    ++ map ([x] :) (partitions xs)

findArrayIndex :: (IA.IArray a e, IA.Ix i) => (e -> Bool) -> a i e -> Maybe i
findArrayIndex f as = fst <$> find (f . snd) (IA.assocs as)
{-# INLINE findArrayIndex #-}

{-- データ構造 --}

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

data SegTree s a = SegTree
  { op :: !(a -> a -> a),
    e :: !a,
    _data :: !(VUM.MVector s a),
    size :: !Int
  }

segTreeFromList :: (VU.Unbox a, PrimMonad m) => (a -> a -> a) -> a -> [a] -> m (SegTree (PrimState m) a)
segTreeFromList op e xs = do
  let size = length xs
  let v = VU.fromList xs
  _data <- VUM.unsafeNew (2 * size)
  for_ [2 * size - 1, 2 * size - 2 .. 1] $ \i -> do
    if i < size
      then do
        x <- VUM.unsafeRead _data (i .<<. 1)
        y <- VUM.unsafeRead _data ((i .<<. 1) + 1)
        VUM.unsafeWrite _data i (x `op` y)
      else do
        let x = v VU.! (i - size)
        VUM.unsafeWrite _data i x
  return $ SegTree op e _data size

segTreeGet :: (VU.Unbox a, PrimMonad m) => SegTree (PrimState m) a -> Int -> m a
segTreeGet SegTree {_data, size} pos = VUM.unsafeRead _data (pos + size)

segTreeSet :: (VU.Unbox a, PrimMonad m) => SegTree (PrimState m) a -> Int -> a -> m ()
segTreeSet SegTree {op, _data, size} pos val = do
  let k = pos + size
  VUM.unsafeWrite _data k val
  let loop !i
        | i == 1 = return ()
        | otherwise = do
            let p = i .>>. 1
            x <- VUM.unsafeRead _data (p .<<. 1)
            y <- VUM.unsafeRead _data ((p .<<. 1) + 1)
            let !newVal = x `op` y
            VUM.unsafeWrite _data p newVal
            loop p
  loop k

segTreeProd :: (VU.Unbox a, PrimMonad m) => SegTree (PrimState m) a -> Int -> Int -> m a
segTreeProd SegTree {op, e, _data, size} l r = do
  let loop !l !r !ansL !ansR
        | l >= r = return (ansL `op` ansR)
        | otherwise = do
            ansL' <- if odd l then do (ansL `op`) <$> VUM.unsafeRead _data l else return ansL
            ansR' <- if odd r then do (`op` ansR) <$> VUM.unsafeRead _data (r - 1) else return ansR
            loop ((l + 1) .>>. 1) (r .>>. 1) ansL' ansR'
  loop (l + size) (r + size) e e

data FenwickTree s a = FenwickTree
  { _data :: !(VUM.MVector s a),
    size :: !Int
  }

newFenwickTree :: (VU.Unbox a, Num a, PrimMonad m) => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = do
  _data <- VUM.replicate (n + 1) 0
  return $ FenwickTree _data n

fenwickTreeAdd :: (VU.Unbox a, Num a, PrimMonad m) => FenwickTree (PrimState m) a -> Int -> a -> m ()
fenwickTreeAdd FenwickTree {_data, size} pos val = do
  let loop !i
        | i > size = return ()
        | otherwise = do
            VUM.unsafeModify _data (+ val) i
            loop (i + (i .&. (-i)))
  loop (pos + 1)

fenwickTreeSum :: (VU.Unbox a, Num a, PrimMonad m) => FenwickTree (PrimState m) a -> Int -> m a
fenwickTreeSum FenwickTree {_data} pos = do
  let loop !i !acc
        | i == 0 = return acc
        | otherwise = do
            x <- VUM.unsafeRead _data i
            loop (i - (i .&. (-i))) (acc + x)
  loop pos 0

data LazySegTree s a f = LazySegTree
  { op :: !(a -> a -> a),
    e :: !a,
    mapping :: !(f -> a -> a),
    composition :: !(f -> f -> f),
    _id :: !f,
    _data :: !(VUM.MVector s a),
    _lazy :: !(VUM.MVector s f),
    size :: !Int
  }

_lazySegTreeEval :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> m a
_lazySegTreeEval LazySegTree {mapping, _data, _lazy} pos = do
  x <- VUM.unsafeRead _data pos
  f <- VUM.unsafeRead _lazy pos
  return $ mapping f x

_lazySegTreePropagate :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> m ()
_lazySegTreePropagate seg@LazySegTree {composition, _id, _data, _lazy} pos = do
  let hs = takeWhile (<= pos) (iterate (.<<. 1) 2)
  for_ (reverse hs) $ \h -> do
    let pos' = pos `div` h
    f <- VUM.unsafeRead _lazy pos'
    val <- _lazySegTreeEval seg pos'
    VUM.unsafeWrite _data pos' val
    VUM.unsafeWrite _lazy pos' _id
    VUM.unsafeModify _lazy (`composition` f) (pos' .<<. 1)
    VUM.unsafeModify _lazy (`composition` f) ((pos' .<<. 1) + 1)

_lazySegTreeUpdate :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> m ()
_lazySegTreeUpdate seg@LazySegTree {op, _data} pos = do
  let hs = takeWhile (<= pos) (iterate (.<<. 1) 2)
  for_ hs $ \h -> do
    let pos' = pos `div` h
    ansL <- _lazySegTreeEval seg (pos' .<<. 1)
    ansR <- _lazySegTreeEval seg ((pos' .<<. 1) + 1)
    VUM.unsafeWrite _data pos' (ansL `op` ansR)

lazySegTreeFromList :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => (a -> a -> a) -> a -> (f -> a -> a) -> (f -> f -> f) -> f -> [a] -> m (LazySegTree (PrimState m) a f)
lazySegTreeFromList op e mapping composition _id xs = do
  let size = length xs
  let v = VU.fromList xs
  _data <- VUM.unsafeNew (2 * size)
  _lazy <- VUM.replicate (2 * size) _id
  for_ [2 * size - 1, 2 * size - 2 .. 1] $ \i -> do
    if i < size
      then do
        x <- VUM.unsafeRead _data (i .<<. 1)
        y <- VUM.unsafeRead _data ((i .<<. 1) + 1)
        VUM.unsafeWrite _data i (x `op` y)
      else do
        let x = v VU.! (i - size)
        VUM.unsafeWrite _data i x
  return $ LazySegTree op e mapping composition _id _data _lazy size

_lazySegTreeApply :: (VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> Int -> f -> m ()
_lazySegTreeApply LazySegTree {composition, _lazy} l r f = do
  let loop !l !r
        | l >= r = return ()
        | otherwise = do
            when (odd l) $ VUM.unsafeModify _lazy (`composition` f) l
            when (odd r) $ VUM.unsafeModify _lazy (`composition` f) (r - 1)
            loop ((l + 1) .>>. 1) (r .>>. 1)
  loop l r

lazySegTreeApply :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> Int -> f -> m ()
lazySegTreeApply seg@LazySegTree {size} a b f = do
  let l = a + size
      r = b + size
      l' = l `div` (l .&. (-l))
      r' = r `div` (r .&. (-r)) - 1
  _lazySegTreePropagate seg l'
  _lazySegTreePropagate seg r'
  _lazySegTreeApply seg l r f
  _lazySegTreeUpdate seg l'
  _lazySegTreeUpdate seg r'

_lazySegTreeProd :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> Int -> m a
_lazySegTreeProd seg@LazySegTree {op, e} l r = do
  let loop !l !r !ansL !ansR
        | l >= r = return (ansL `op` ansR)
        | otherwise = do
            ansL' <- if odd l then do (ansL `op`) <$> _lazySegTreeEval seg l else return ansL
            ansR' <- if odd r then do (`op` ansR) <$> _lazySegTreeEval seg (r - 1) else return ansR
            loop ((l + 1) .>>. 1) (r .>>. 1) ansL' ansR'
  loop l r e e

lazySegTreeProd :: (VU.Unbox a, VU.Unbox f, PrimMonad m) => LazySegTree (PrimState m) a f -> Int -> Int -> m a
lazySegTreeProd seg@LazySegTree {size} a b = do
  let l = a + size
      r = b + size
      l' = l `div` (l .&. (-l))
      r' = r `div` (r .&. (-r)) - 1
  _lazySegTreePropagate seg l'
  _lazySegTreePropagate seg r'
  _lazySegTreeProd seg l r

{-- デバッグ --}

#ifndef ATCODER

dbg :: (Show a) => a -> ()
dbg = (`traceShow` ())

#else

dbg :: (Show a) => a -> ()
dbg = const ()

#endif
