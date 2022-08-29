module Chunk where

import Data.DList
import Data.Word

import Value

data Chunk = Chunk {
    count :: Int
  , code  :: DList Word8
  , lines :: Int
  , linesCount :: Int
  , constants :: ValueArray
}

initChunk :: Chunk
initChunk = Chunk 0 empty 0 0 initValueArray
