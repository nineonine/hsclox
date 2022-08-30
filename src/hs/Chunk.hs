{-# LANGUAGE OverloadedRecordDot #-}
module Chunk where

import Data.DList
import Data.Int
import Data.Word

import Value

data Chunk = Chunk {
    count :: Int -- is this needed?
  , code  :: DList Word8
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
