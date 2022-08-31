{-# LANGUAGE OverloadedRecordDot #-}
module Chunk where

import Data.Int
import Data.Vector
import Data.Word

import Value

data Chunk = Chunk {
    count :: Int
  , code  :: Vector Word8
  , lines :: Int
  , linesCount :: Int
  , constants :: ValueArray
}

initChunk :: Chunk
initChunk = Chunk 0 empty 0 0 initValueArray

writeChunk :: Chunk -> Word8 -> Int64 -> Chunk
writeChunk chunk byte line
    | code' <- snoc chunk.code byte
    = updateLineInfo line $ -- TODO
      chunk { code = code' }

updateLineInfo :: Int64 -> Chunk -> Chunk
updateLineInfo _ = id
