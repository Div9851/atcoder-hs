{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module BufferQueue (Buffer, newBufferAsQueue, newBufferAsDeque, pushFrontBuf, pushBackBuf, popFrontBuf) where

import Control.Exception (assert)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

data Buffer s a = Buffer
  { bufferVars :: !(VUM.MVector s Int),
    internalBuffer :: !(VUM.MVector s a),
    internalBufferSize :: !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

newBufferAsQueue :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsQueue n = Buffer <$> VUM.replicate 2 0 <*> VUM.unsafeNew n <*> pure n

newBufferAsDeque :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsDeque n =
  Buffer
    <$> VUM.replicate 2 n
    <*> VUM.unsafeNew (2 * n)
    <*> pure (2 * n)

popFrontBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFrontBuf Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      VUM.unsafeWrite bufferVars _bufferFrontPos (f + 1)
      pure <$> VUM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE popFrontBuf #-}

pushFrontBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFrontBuf Buffer {bufferVars, internalBuffer} x = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  VUM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  assert (f > 0) $ do
    VUM.unsafeWrite internalBuffer (f - 1) x
{-# INLINE pushFrontBuf #-}

pushBackBuf :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBackBuf Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VUM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  assert (b < internalBufferSize) $ do
    VUM.unsafeWrite internalBuffer b x
{-# INLINE pushBackBuf #-}
