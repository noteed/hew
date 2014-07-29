{-# LANGUAGE BangPatterns #-}
-- | Rolling checksum algorithm from Bup (itself inspired by Rsync). The
-- `roll` function definition layout is similar to the Edward Kmett's `roll`
-- function from the `hash` package.
module Main (main) where

import Data.Bits
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.Monoid
import Data.Int
import Data.Word
import System.Environment (getArgs)

main :: IO ()
main = do
  [fn] <- getArgs
  is <- L.readFile fn
  let pxs = Prelude.zip [0..] $ roll' is
  Prelude.putStrLn $ "Number of chunks: " ++ show (Prelude.length pxs)
  mapM_ (\(p, x) -> B.writeFile (fn ++ "." ++ show p ++ ".chunk") $ fst x) pxs
  return ()

bupWindowBits :: Int
bupWindowBits = 6

bupWindowSize :: Word32
bupWindowSize = bit bupWindowBits

bupBlobBits :: Int
bupBlobBits = 13

bupBlobSize :: Word32
bupBlobSize = bit bupBlobBits

rollsumCharOffset :: Word32
rollsumCharOffset = 31

roll :: L.ByteString -> L.ByteString
roll z = L.fromChunks $
  step s1_ s2_ 0 z (L.unpack (L.replicate (fromIntegral bupWindowSize) 0 <> z)) (L.unpack z)
  where
  s1_ = bupWindowSize * rollsumCharOffset
  s2_ = bupWindowSize * (bupWindowSize - 1) * rollsumCharOffset

  step :: Word32 -> Word32 -> Int64 -> L.ByteString -> [Word8] -> [Word8] -> [B.ByteString]
  step s1 s2 c bs (x:xs) (y:ys)
    | s2' .&. (bupBlobSize - 1) == complement 0 .&. (bupBlobSize - 1) =
      case L.splitAt (c + 1) bs of
        (l, r) -> B.concat (L.toChunks l) : step s1_ s2_ 0 r xs ys
    | otherwise = step s1' s2' (c + 1) bs xs ys
    where s1' = s1 + (fromIntegral y) - (fromIntegral x)
          s2' = s2 + s1' - (bupWindowSize * (fromIntegral x + rollsumCharOffset))
  step _ _ _ bs _ _ = [B.concat $ L.toChunks bs]

roll' :: L.ByteString -> [(B.ByteString, Int)]
roll' z =
  step s1_ s2_ 0 z (L.unpack (L.replicate (fromIntegral bupWindowSize) 0 <> z)) (L.unpack z)
  where
  s1_ = bupWindowSize * rollsumCharOffset
  s2_ = bupWindowSize * (bupWindowSize - 1) * rollsumCharOffset

  step :: Word32 -> Word32 -> Int64 -> L.ByteString -> [Word8] -> [Word8] -> [(B.ByteString, Int)]
  step !s1 !s2 !c !bs (x:xs) (y:ys)
    | s2' .&. (bupBlobSize - 1) == complement 0 .&. (bupBlobSize - 1) =
      case L.splitAt (c + 1) bs of
        (l, r) -> (B.concat (L.toChunks l), countBits s1' s2') : step s1_ s2_ 0 r xs ys
    | otherwise = step s1' s2' (c + 1) bs xs ys
    where s1' = s1 + (fromIntegral y) - (fromIntegral x)
          s2' = s2 + s1' - (bupWindowSize * (fromIntegral x + rollsumCharOffset))
  step _ _ _ bs _ _ = [(B.concat $ L.toChunks bs, 0)]

rollsumDigest :: Word32 -> Word32 -> Word32
rollsumDigest s1 s2 = (s1 `unsafeShiftL` 16) .|. (s2 .&. 0xffff)

countBits :: Word32 -> Word32 -> Int
countBits s1 s2 = bupBlobBits + inc 0 rsum
  where
  rsum = rollsumDigest s1 s2 `unsafeShiftR` bupBlobBits
  inc i s = if s' .&. 1 /= 0 then inc (i + 1) s' else i
    where s' = s `unsafeShiftR` 1
