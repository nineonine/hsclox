{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler where

import Data.Int
import Data.Word
import Control.Monad.Trans.State.Strict
import Data.Map.Strict

import Object
import OpCode
import Parser
import Token
import Value

-- The bytecode compiler / assembler

foreign export ccall compileFromHs :: IO ()

compileFromHs :: IO ()
compileFromHs = print "compiling in hs ..."

type CompilerT = StateT CompilerState IO

data CompilerState = CompilerState {
    parser       :: !Parser
  , current      :: !Compiler
  , currentClass :: !ClassCompiler
}

type ParseFn = Bool -> CompilerT ()

type Globals = Map String () -- Value

data Compiler = Compiler {
    enclosing :: Compiler
  , function :: ObjFunction
  , funType  :: FunctionType
  , locals :: [Local]
  , globals :: Globals
  , localCount :: Int64
  , loop :: Loop
}

data ClassCompiler = ClassCompiler {
    hasSuperClass :: !Bool
  , enclosing     :: !ClassCompiler
}

currentChunk :: CompilerT Chunk
currentChunk  = do
    CompilerState{..} <- get
    return (chunk (function current))

errotAt :: Token -> String -> CompilerT ()
errotAt token msg = return ()

error :: String -> CompilerT ()
error msg = return ()

errorAtCurrent :: String -> CompilerT ()
errorAtCurrent msg = return ()

advance :: CompilerT ()
advance = return ()

consume :: TokenType -> String -> CompilerT ()
consume tokenType msg = return ()

check :: TokenType -> CompilerT Bool
check tokenType = return False

match :: TokenType -> CompilerT ()
match tokenType = return ()

emitByte :: Word8 -> CompilerT ()
emitByte byte = return ()

emitBytes :: Word8 -> Word8 -> CompilerT ()
emitBytes byte1 byte2 = return ()

emitLoop :: Int -> CompilerT ()
emitLoop loopStart = return ()

emitJump :: Word8 -> CompilerT Int
emitJump  instruction = return 1

emitReturn :: CompilerT ()
emitReturn = return ()

makeConstant :: Value -> CompilerT Word8
makeConstant value = return 1

emitConstant :: Value -> CompilerT ()
emitConstant value = do
  c <- makeConstant value
  emitBytes (toBytes OP_CONSTANT) c
