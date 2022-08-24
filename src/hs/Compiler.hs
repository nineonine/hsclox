{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler where

import Data.Int
import Control.Monad.Trans.State.Strict
import Data.Map.Strict

import Object
import Parser
import Token

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
