{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}

import Control.Monad.ST (runST)
import System.Random (mkStdGen,randoms)
import Data.Primitive (ByteArray(ByteArray),unsafeFreezeByteArray)
import Data.Primitive (copyByteArray,newPinnedByteArray)
import Gauge.Main (defaultMain,bgroup,bench,whnf)
import Data.ByteString.Internal (ByteString(PS))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Base64 as Base64
import qualified Data.List as List
import qualified GHC.Exts as Exts
import qualified GHC.ForeignPtr as Exts

import qualified "base64-bytestring" Data.ByteString.Base64 as BS
import qualified "base64" Data.ByteString.Base64 as B64

main :: IO ()
main = defaultMain
  [ bgroup "encode"
    [ bgroup "base64-bytes"
      [ bench "100" $
          whnf (\x -> Base64.encode (Bytes.fromByteArray x)) str100
      , bench "1000" $
          whnf (\x -> Base64.encode (Bytes.fromByteArray x)) str1000
      , bench "10000" $
          whnf (\x -> Base64.encode (Bytes.fromByteArray x)) str10000
      ]
    , bgroup "base64-bytestring"
      [ bench "100" $
          whnf (\x -> BS.encode (byteArrayToByteString 100 x)) str100
      , bench "1000" $
          whnf (\x -> BS.encode (byteArrayToByteString 1000 x)) str1000
      , bench "10000" $
          whnf (\x -> BS.encode (byteArrayToByteString 10000 x)) str10000
      ]
    , bgroup "base64"
      [ bench "100" $
          whnf (\x -> B64.encodeBase64' (byteArrayToByteString 100 x)) str100
      , bench "1000" $
          whnf (\x -> B64.encodeBase64' (byteArrayToByteString 1000 x)) str1000
      , bench "10000" $
          whnf (\x -> B64.encodeBase64' (byteArrayToByteString 10000 x)) str10000
      ]
    ]
  ]

byteArrayToByteString :: Int -> ByteArray -> ByteString
{-# inline byteArrayToByteString #-}
byteArrayToByteString len (ByteArray x) = PS
  ( Exts.ForeignPtr
    (Exts.byteArrayContents# x)
    (Exts.PlainPtr (Exts.unsafeCoerce# x))
  ) 0 len

str100 :: ByteArray
{-# NOINLINE str100 #-}
str100 = mkPinned 100

str1000 :: ByteArray
{-# NOINLINE str1000 #-}
str1000 = mkPinned 1000

str10000 :: ByteArray
{-# NOINLINE str10000 #-}
str10000 = mkPinned 10000

mkPinned :: Int -> ByteArray
mkPinned n = runST do
  x <- newPinnedByteArray n
  let src = Exts.fromList (List.take n (randoms (mkStdGen 42)))
  copyByteArray x 0 src 0 n
  unsafeFreezeByteArray x
