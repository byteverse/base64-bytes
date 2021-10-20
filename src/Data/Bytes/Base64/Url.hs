{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Data.Bytes.Base64.Url
  ( decode64
  ) where

import Data.Bytes (Bytes)
import GHC.Exts (Ptr(Ptr))
import Data.Word (Word8,Word64)
import Data.Bits (unsafeShiftL,(.|.))

import qualified Data.Bytes as Bytes
import qualified Data.Primitive.Ptr as PM

-- Decode a base64-url-encoded 64-bit word. Rejects encoded numbers
-- greater than or equal to @2^64@. This maps the rightmost byte to
-- the 6 least significant bits of the word. 
decode64 :: Bytes -> Maybe Word64
decode64 bs
  | Bytes.length bs > 10 = Nothing
  | otherwise = Bytes.foldlM
      (\ !(acc :: Word64) b -> case PM.indexOffPtr table (fromIntegral @Word8 @Int b) of
        0xFF -> Nothing
        w -> pure $! (unsafeShiftL acc 6 .|. fromIntegral @Word8 @Word64 w)
      ) 0 bs

table :: Ptr Word8
table = Ptr
  "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\
  \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\x63\xff\xff\
  \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
  \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\x3f\
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