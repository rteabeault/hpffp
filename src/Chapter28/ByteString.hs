{-# LANGUAGE OverloadedStrings #-}

module Chapter28.ByteString where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL -- https://hackage.haskell.org/package/zlib
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

main :: IO ()
main = do
  TIO.putStrLn $ TE.decodeUtf8 (s input)
  -- This fails because gzip will contain some non-text bytes
  TIO.putStrLn $ TE.decodeUtf8 (s compressed)
  -- The encoding module in the text library expects strict ByteStrings,
  -- so we have to make them strict before attempting a decoding.
  where s = BL.toStrict
