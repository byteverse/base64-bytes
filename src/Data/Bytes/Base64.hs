{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Bytes.Base64
  ( encode
  , builder
  , recodeBoundedBuilder
  , decode64
  ) where

import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray (..), MutableByteArray (..), newByteArray, readByteArray, unsafeFreezeByteArray)
import Data.Primitive.Ptr (indexOffPtr)
import Data.Word (Word64, Word8)
import GHC.Exts (Int (I#), Ptr (Ptr), State#, (+#), (-#))
import GHC.ST (ST (ST))
import GHC.TypeNats (Div, type (*), type (+))
import GHC.Word (Word (W#), Word32)
import GHC.Word.Compat (pattern W32#)

import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder.Bounded.Unsafe as BBU
import qualified Data.Bytes.Builder.Unsafe as BU
import qualified Data.Primitive.ByteArray.BigEndian as BE
import qualified Data.Primitive.ByteArray.LittleEndian as LE
import qualified Data.Primitive.Ptr as PM
import qualified GHC.Exts as Exts

-- | Encode a byte sequence with base64.
encode :: Bytes -> ByteArray
encode (Bytes src soff slen) = runByteArrayST do
  let dlen = calculatePaddedLength slen
  dst <- newByteArray dlen
  performEncodeImmutable dst 0 src soff slen
  unsafeFreezeByteArray dst

-- | Encode a byte sequence with base64 as a builder.
builder :: Bytes -> BU.Builder
builder (Bytes src soff slen) = BU.fromEffect dlen \dst doff -> do
  performEncodeImmutable dst doff src soff slen
  pure (doff + dlen)
 where
  dlen = calculatePaddedLength slen

{- | Encode a byte sequence with base64. This augments a builder
by first playing the original builder to paste the byte sequence
and then performing the base64 encoding in-place on the buffer
that had been pasted into.
-}
recodeBoundedBuilder ::
  Arithmetic.Nat n ->
  BBU.Builder n ->
  BBU.Builder (4 * (Div (n + 2) 3))
recodeBoundedBuilder !n (BBU.Builder f) =
  BBU.Builder
    ( \arr off0 s0 ->
        let !off1 = (off0 +# maxEncLen) -# maxRawLen
         in case f arr off1 s0 of
              (# s1, off2 #) ->
                let !actualLen = off2 -# off1
                 in case unST (performEncode (MutableByteArray arr) (I# off0) (MutableByteArray arr) (I# off1) (W# (Exts.int2Word# actualLen))) s1 of
                      (# s2, (_ :: ()) #) ->
                        let !(I# actualEncLen) = calculatePaddedLength (I# actualLen)
                         in (# s2, actualEncLen #)
    )
 where
  !(I# maxRawLen) = Nat.demote n
  !(I# maxEncLen) = calculatePaddedLength (I# maxRawLen)

performEncodeImmutable ::
  MutableByteArray s -> -- dest
  Int -> -- dest off
  ByteArray ->
  Int -> -- src off
  Int -> -- source length
  ST s ()
performEncodeImmutable dst doff (ByteArray src) soff slen =
  performEncode dst doff (MutableByteArray (Exts.unsafeCoerce# src)) soff (fromIntegral @Int @Word slen)

-- The function is the meat of this module. Implementation notes:
--
-- We use big-endian and little-endian unaligned loads and stores
-- from the byte-order library. This means we can cut down loads
-- and stores by a factor of 4.
--
-- Once we get down to less than 4 bytes, we have to do some grunt
-- work to finish off the encoding. This happens for two reasons.
-- The first is that base64 requires trailing equals signs to pad
-- encoded byte sequences whose length was not a multiple of three
-- The second is that our 32-bit load is unsafe once we are at the
-- end since its possible (although really unlikely) that the byte
-- sequences is right up against the end of the address space that
-- is available to GHC. Segfaults happen if we wander outside of
-- this pasture.
--
-- Why is the source a mutable byte array? We actually need this
-- to accept both immutable and mutable byte arrays as the source.
-- To avoid code duplication, we use unsafeCoerce# in performEncodeImmutable.
-- Using the mutable variant here actually gives us slightly better
-- guarantees from the compiler since read (unlike index) is sequenced.
-- These guarantees are important in recodeBoundedBuilder, where the
-- encoding is performed in-place.
--
-- Also, what's the deal with the source length being a Word instead
-- of an Int. GHC can actually generate code when we do this.
-- In the cmm stage of compilation, case-on-number constructs
-- are compiled to lower-level constructs. They become either jump
-- table or a series of conditionals statements. In our case,
-- an unsigned number helps GHC realize that it does not need
-- to test for n<0, although it still must test for n>3.
performEncode ::
  MutableByteArray s -> -- dest
  Int -> -- dest off
  MutableByteArray s -> -- src
  Int -> -- src off
  Word -> -- source length
  ST s ()
performEncode !dst !doff !src !soff !slen = case slen of
  3 -> do
    x1 <- readByteArray src soff
    x2 <- readByteArray src (soff + 1)
    x3 <- readByteArray src (soff + 2)
    let (w1, w2, w3, w4) = disassembleBE (assembleBE x1 x2 x3 0)
        c1 = indexOffPtr table (fromIntegral @Word @Int w1)
        c2 = indexOffPtr table (fromIntegral @Word @Int w2)
        c3 = indexOffPtr table (fromIntegral @Word @Int w3)
        c4 = indexOffPtr table (fromIntegral @Word @Int w4)
    LE.writeUnalignedByteArray dst doff (assembleLE c1 c2 c3 c4)
  2 -> do
    x1 <- readByteArray src soff
    x2 <- readByteArray src (soff + 1)
    let (w1, w2, w3, _) = disassembleBE (assembleBE x1 x2 0 0)
        c1 = indexOffPtr table (fromIntegral @Word @Int w1)
        c2 = indexOffPtr table (fromIntegral @Word @Int w2)
        c3 = indexOffPtr table (fromIntegral @Word @Int w3)
        c4 = c2w '='
    LE.writeUnalignedByteArray dst doff (assembleLE c1 c2 c3 c4)
  1 -> do
    x1 <- readByteArray src soff
    let (w1, w2, _, _) = disassembleBE (assembleBE x1 0 0 0)
        c1 = indexOffPtr table (fromIntegral @Word @Int w1)
        c2 = indexOffPtr table (fromIntegral @Word @Int w2)
        c3 = c2w '='
        c4 = c2w '='
    LE.writeUnalignedByteArray dst doff (assembleLE c1 c2 c3 c4)
  0 -> pure ()
  _ -> do
    -- This last case is always slen > 3
    w :: Word32 <- BE.readUnalignedByteArray src soff
    let (w1, w2, w3, w4) = disassembleBE w
        c1 = indexOffPtr table (fromIntegral @Word @Int w1)
        c2 = indexOffPtr table (fromIntegral @Word @Int w2)
        c3 = indexOffPtr table (fromIntegral @Word @Int w3)
        c4 = indexOffPtr table (fromIntegral @Word @Int w4)
    LE.writeUnalignedByteArray dst doff (assembleLE c1 c2 c3 c4)
    performEncode dst (doff + 4) src (soff + 3) (slen - 3)

-- Argument bytes are hi to lo. The first argument becomes
-- the least significant component of the result.
assembleLE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
assembleLE a b c d =
  unsafeW32
    ( unsafeShiftL (fromIntegral @Word8 @Word d) 24
        .|. unsafeShiftL (fromIntegral @Word8 @Word c) 16
        .|. unsafeShiftL (fromIntegral @Word8 @Word b) 8
        .|. (fromIntegral @Word8 @Word a)
    )

-- Argument bytes are hi to lo. The first argument becomes
-- the most significant component of the result.
assembleBE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
assembleBE a b c d =
  unsafeW32
    ( unsafeShiftL (fromIntegral @Word8 @Word a) 24
        .|. unsafeShiftL (fromIntegral @Word8 @Word b) 16
        .|. unsafeShiftL (fromIntegral @Word8 @Word c) 8
        .|. (fromIntegral @Word8 @Word d)
    )

-- Create a 32-bit word from a machine word that we know
-- is not too large.
unsafeW32 :: Word -> Word32
unsafeW32 (W# w) = W32# w

-- We only care about the upper 24 bits of the argument.
-- This gets broken into four 6-bit words.
disassembleBE :: Word32 -> (Word, Word, Word, Word)
disassembleBE !w =
  ( unsafeShiftR (fromIntegral @Word32 @Word w) 26
  , unsafeShiftR (fromIntegral @Word32 @Word w) 20 .&. 0b00111111
  , unsafeShiftR (fromIntegral @Word32 @Word w) 14 .&. 0b00111111
  , unsafeShiftR (fromIntegral @Word32 @Word w) 8 .&. 0b00111111
  )

table :: Ptr Word8
table = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

calculatePaddedLength :: Int -> Int
calculatePaddedLength n = 4 * (divRoundUp n 3)

divRoundUp :: Int -> Int -> Int
divRoundUp x y = div (x + y - 1) y

c2w :: Char -> Word8
c2w = fromIntegral . ord

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) s = f s

-- Decode a base64-url-encoded 64-bit word. Rejects encoded numbers
-- greater than or equal to @2^64@. This maps the rightmost byte to
-- the 6 least significant bits of the word.
decode64 :: Bytes -> Maybe Word64
decode64 bs
  | Bytes.length bs > 10 = Nothing
  | otherwise =
      Bytes.foldlM
        ( \ !(acc :: Word64) b -> case PM.indexOffPtr decodeTable (fromIntegral @Word8 @Int b) of
            0xFF -> Nothing
            w -> pure $! (unsafeShiftL acc 6 .|. fromIntegral @Word8 @Word64 w)
        )
        0
        bs

decodeTable :: Ptr Word8
decodeTable =
  Ptr
    "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\xff\x3f\
    \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\x63\xff\xff\
    \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
    \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\xff\
    \\xff\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\
    \\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
