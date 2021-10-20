{-# language BangPatterns #-}
{-# language LambdaCase #-}

import Control.Monad (when)
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Numeric (showHex)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Data.Bytes.Base64 as Base64
import qualified Data.Bytes.Base64.Url as Base64Url

main :: IO ()
main = do
  putStr "Base 64 Encoding\n"
  putStr "================\n"
  putStr "Encoding: foobar\n"
  putStr "Expected: 5a6d3976596d4679 (Zm9vYmFy)\n"
  putStr "Got:      "
  printHex actualFoobar
  when (actualFoobar /= expectedFoobar) (fail "Did not match")
  putStr "Encoding: helloworld\n"
  putStr "Expected: 614756736247393362334a735a413d3d (aGVsbG93b3JsZA==)\n"
  putStr "Got:      "
  printHex actualHelloworld
  when (actualHelloworld /= expectedHelloworld) (fail "Did not match")
  putStr "Encoding: camel\n"
  putStr "Expected: 593246745a57773d (Y2FtZWw=)\n"
  putStr "Got:      "
  printHex actualCamel
  when (actualCamel /= expectedCamel) (fail "Did not match")
  putStr "Encoding: 123.6789\n"
  putStr "Expected: 4d54497a4c6a59334f446b3d (MTIzLjY3ODk=)\n"
  putStr "Got:      "
  printHex actualNumbers
  when (actualNumbers /= expectedNumbers) (fail "Did not match")
  putStr "Base 64 URL Decoding\n"
  putStr "====================\n"
  putStr "Decoding: _\n"
  putStr "Expected: 63\n"
  underscoreDecodeResult <- case Base64Url.decode64 (Bytes.singleton (c2w '_')) of
    Nothing -> fail "Could not decode"
    Just w -> pure w
  putStr ("Got:      " ++ show underscoreDecodeResult ++ "\n")
  when (underscoreDecodeResult /= 63) $
    (fail "Base64 URL decode of single underscore failed")
  putStr "Decoding: _-a\n"
  putStr "Expected: 262042\n"
  urlResB <- case Base64Url.decode64 (Latin1.fromString "_-a") of
    Nothing -> fail "Could not decode"
    Just w -> pure w
  putStr ("Got:      " ++ show urlResB ++ "\n")
  when (urlResB /= 262042) $
    (fail "Base64 URL decode of _-a failed")
  putStr "\nAll tests succeeded!\n"

printHex :: ByteArray -> IO ()
printHex !b = putStr (go 0) where
  go !ix = if ix < PM.sizeofByteArray b
    then let val = PM.indexByteArray b ix :: Word8 in
      if val < 16
        then '0' : showHex val (go (ix + 1))
        else showHex val (go (ix + 1))
    else "\n"

actualFoobar :: ByteArray
actualFoobar = Base64.encode (Latin1.fromString "foobar")

actualHelloworld :: ByteArray
actualHelloworld = Base64.encode (Latin1.fromString "helloworld")

actualCamel :: ByteArray
actualCamel = Base64.encode (Latin1.fromString "camel")

actualNumbers :: ByteArray
actualNumbers = BB.run Nat.constant
  ( Base64.recodeBoundedBuilder
    Nat.constant
    (BB.wordDec 123 `BB.append` BB.ascii '.' `BB.append` BB.wordDec 6789)
  )

expectedFoobar :: ByteArray
expectedFoobar = Exts.fromList [0x5a,0x6d,0x39,0x76,0x59,0x6d,0x46,0x79]

expectedHelloworld :: ByteArray
expectedHelloworld = Exts.fromList
  [0x61,0x47,0x56,0x73,0x62,0x47,0x39,0x33
  ,0x62,0x33,0x4a,0x73,0x5a,0x41,0x3d,0x3d
  ]

expectedCamel :: ByteArray
expectedCamel = Exts.fromList [0x59,0x32,0x46,0x74,0x5a,0x57,0x77,0x3d]

expectedNumbers :: ByteArray
expectedNumbers = Exts.fromList
  [0x4d,0x54,0x49,0x7a,0x4c,0x6a
  ,0x59,0x33,0x4f,0x44,0x6b,0x3d
  ]

c2w :: Char -> Word8
c2w = fromIntegral . ord
