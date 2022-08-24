{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler where

import Data.Int
import Control.Monad.Trans.State.Strict
import Data.Map.Strict

import Obj
import Parser

-- The bytecode compiler / assembler

foreign export ccall compileFromHs :: IO ()

compileFromHs :: IO ()
compileFromHs = print "compiling in hs ..."

data CompilerState = CompilerState {
    parser       :: !Parser
  , current      :: !Compiler
  , currentClass :: !ClassCompiler
}

type CompilerT = StateT CompilerState IO

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
