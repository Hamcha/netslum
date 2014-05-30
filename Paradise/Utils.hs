{-# LANGUAGE OverloadedStrings #-}

module Paradise.Utils
( stripHTML
, clearScreenCode
, boldCode
, normalCode
, reverseCode
) where

import qualified Data.ByteString.Char8 as B
import           Text.Regex.PCRE.ByteString.Utils

stripHTML :: B.ByteString -> IO B.ByteString
stripHTML html = do
    blk  <- substituteCompile "</?(p|li|ul)[^>]*>" html ""
    ves1 <- substituteCompile "<vessel[^>]*>" (getByte blk)  reverseCode
    ves2 <- substituteCompile "</vessel>"     (getByte ves1) normalCode
    sub1 <- substituteCompile "<[^/>]*>"      (getByte ves2) boldCode
    sub2 <- substituteCompile "</[^>]*>"      (getByte sub1) normalCode
    return $ getByte sub2

clearScreenCode :: B.ByteString
clearScreenCode = "\ESC[2J"

boldCode :: B.ByteString
boldCode = "\ESC[1m"

normalCode :: B.ByteString
normalCode = "\ESC[0m"

reverseCode :: B.ByteString
reverseCode = "\ESC[7m"

getByte :: Either String B.ByteString -> B.ByteString
getByte (Left x)  = B.pack x
getByte (Right x) = x