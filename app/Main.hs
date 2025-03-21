{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Exception (assert)
import Control.Monad (filterM, forM, forM_, mapM, mapM_, unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (modify', runStateT)
import Data.Array.Base (STUArray, newArray, readArray, writeArray)
import Data.ByteString.Char8 qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

main :: IO ()
main = do
  putStrLn "Hello"

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
